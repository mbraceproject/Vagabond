//
//  Parse a loaded System.Reflection.Assembly into Cecil's data structures.
//
//  Code adapted from JB Evain's AssemblySaver: https://github.com/jbevain/mono.reflection/tree/assembly-saver
//

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;

using SR = System.Reflection;
using MR = Mono.Reflection;
using MC = Mono.Cecil;

using Mono.Reflection;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace MBrace.Vagabond.AssemblyParser
{
    /// <summary>
    /// Dynamic assembly to Cecil tree parser implementation.
    /// </summary>
    public class AssemblyParser
    {
        private const BindingFlags AllDeclared = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance | BindingFlags.DeclaredOnly;

        /// <summary>
        /// Parse assembly using specified Assembly.
        /// </summary>
        /// <param name="assembly">Assembly to be parsed.</param>
        /// <param name="options">Parsing options configuration object.</param>
        /// <returns>Cecil Assembly tree.</returns>
        public static AssemblyDefinition Parse(Assembly assembly, IAssemblyParserConfig options)
        {
            var mapper = new AssemblyParser(assembly, options);
            return mapper.Map();
        }

        /// <summary>
        /// Parse assembly using specified Assembly.
        /// </summary>
        /// <param name="assembly">Assembly to be parsed.</param>
        /// <returns>Cecil Assembly tree.</returns>
        public static AssemblyDefinition Parse(Assembly assembly)
        {
            var options = new DefaultAssemblyParserConfig();
            return AssemblyParser.Parse(assembly, options);
        }

        private readonly Assembly _assembly;
        private readonly IAssemblyParserConfig _options;

        private AssemblyDefinition _assembly_definition;
        private ModuleDefinition _module_definition;
        private bool _is_running_mono;
        

        private AssemblyParser(Assembly assembly, IAssemblyParserConfig options)
        {
            _assembly = assembly;
            _options = options;
            _is_running_mono = System.Type.GetType("Mono.Runtime") != null;
        }

        private AssemblyDefinition Map()
        {
            _assembly_definition = AssemblyDefinitionFor(_assembly);
            _module_definition = _assembly_definition.MainModule;

            foreach (var type in _assembly.GetTypes())
                if (! type.IsNested) MapType(type);

            MapCustomAttributes(_assembly, _assembly_definition);
            MapCustomAttributes(_assembly.ManifestModule, _assembly_definition.MainModule);

            return _assembly_definition;
        }

        private void MapType(Type type, TypeDefinition declaringType = null)
        {
            switch (_options.GetTypeParseAction(type))
            {
                case TypeParseAction.Ignore:
                    break;

                case TypeParseAction.ParseNested:
                    var _type_definition = TypeDefinitionFor(type, declaringType);

                    foreach (var nested_type in type.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic))
                        MapType(nested_type, _type_definition);

                    break;

                case TypeParseAction.ParseAll:
                    var type_definition = TypeDefinitionFor(type, declaringType);

                    foreach (var field in type.GetFields(AllDeclared))
                        MapField(type_definition, field);

                    foreach (var method in type.GetConstructors(AllDeclared))
                        MapMethod(type_definition, method);

                    foreach (var method in type.GetMethods(AllDeclared))
                        MapMethod(type_definition, method);

                    foreach (var property in type.GetProperties(AllDeclared))
                        MapProperty(property, PropertyDefinitionFor(property, type_definition));

                    foreach (var evt in type.GetEvents(AllDeclared))
                        MapEvent(evt, EventDefinitionFor(evt, type_definition));

                    foreach (var iface in type.GetInterfaces())
                        type_definition.Interfaces.Add(new InterfaceImplementation(CreateReference(iface, type_definition)));

                    foreach (var nested_type in type.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic))
                        MapType(nested_type, type_definition);

                    MapCustomAttributes(type, type_definition);
                    break;
            }
        }

        private void MapMethod(TypeDefinition type_definition, MethodBase method)
        {
            if (_options.EraseMember(method)) return;

            var method_definition = MethodDefinitionFor(method, type_definition);

            MapCustomAttributes(method, method_definition);
            MapOverrides(method, method_definition);
            MapPInvokeInfo(method, method_definition);

            if (!ShouldMapBody(method, method_definition))
                return;

            MapMethodBody(method, method_definition);
        }

        private void MapPInvokeInfo(MethodBase method, MethodDefinition method_definition)
        {
            var attributes = method.GetCustomAttributes(typeof(DllImportAttribute), inherit: false);
            if (attributes.Length == 0)
                return;

            var import = (DllImportAttribute)attributes[0];
            var info = new PInvokeInfo(0, import.EntryPoint, ModuleReferenceFor(import.Value))
            {
                IsBestFitEnabled = import.BestFitMapping,
                IsThrowOnUnmappableCharEnabled = import.ThrowOnUnmappableChar,
                SupportsLastError = import.SetLastError,
                IsNoMangle = import.ExactSpelling,
            };

            switch (import.CallingConvention)
            {
                case CallingConvention.Cdecl:
                    info.IsCallConvCdecl = true;
                    break;
                case CallingConvention.FastCall:
                    info.IsCallConvFastcall = true;
                    break;
                case CallingConvention.StdCall:
                    info.IsCallConvStdCall = true;
                    break;
                case CallingConvention.ThisCall:
                    info.IsCallConvThiscall = true;
                    break;
                case CallingConvention.Winapi:
                    info.IsCallConvWinapi = true;
                    break;
            }

            switch (import.CharSet)
            {
                case CharSet.Ansi:
                    info.IsCharSetAnsi = true;
                    break;
                case CharSet.Auto:
                    info.IsCharSetAuto = true;
                    break;
                case CharSet.None:
                    info.IsCharSetNotSpec = true;
                    break;
                case CharSet.Unicode:
                    info.IsCharSetUnicode = true;
                    break;
            }

            method_definition.PInvokeInfo = info;
        }

        private ModuleReference ModuleReferenceFor(string name)
        {
            foreach (var reference in _module_definition.ModuleReferences)
                if (reference.Name == name)
                    return reference;

            var module = new ModuleReference(name);
            _module_definition.ModuleReferences.Add(module);
            return module;
        }

        private static bool ShouldMapBody(MethodBase method, MethodDefinition method_definition)
        {
            return method_definition.HasBody && method.GetMethodBody() != null;
        }

        private void MapOverrides(MethodBase method, MethodDefinition method_definition)
        {
            var mi = method as MethodInfo;
            if (mi == null || !mi.IsVirtual)
                return;

            var type = method.DeclaringType;
            if (type == null || type.IsInterface)
                return;

            var overrides = type
                .GetInterfaces()
                .Select(type.GetInterfaceMap)
                .SelectMany(m => m.InterfaceMethods.Zip(m.TargetMethods, (im, tm) => new { InterfaceMethod = im, TargetMethod = tm }))
                .Where(p => p.TargetMethod.DeclaringType == type)
                .Where(p => p.InterfaceMethod.Name != p.TargetMethod.Name)
                .Where(p => p.TargetMethod == method)
                .Select(p => p.InterfaceMethod);

            foreach (var ov in overrides)
                method_definition.Overrides.Add(CreateReference(ov, method_definition).GetElementMethod());
        }

        private void MapField(TypeDefinition type_definition, FieldInfo field)
        {
            if (_options.EraseMember(field)) return;

            var field_definition = FieldDefinitionFor(field, type_definition);

            if (field_definition.HasDefault)
                field_definition.Constant = field.GetRawConstantValue();

            if ((field_definition.Attributes & MC.FieldAttributes.HasFieldRVA) != 0)
                field_definition.InitialValue = GetInitialValue(field);

            var attributes = field.GetCustomAttributes(typeof(FieldOffsetAttribute), inherit: false);
            if (attributes.Length > 0)
                field_definition.Offset = ((FieldOffsetAttribute)attributes[0]).Value;

            MapCustomAttributes(field, field_definition);
        }

        private static byte[] GetInitialValue(FieldInfo field)
        {
            if (!field.IsStatic)
                throw new NotSupportedException();

            var value = field.GetValue(null);
            if (value == null)
                return new byte[0];

            var type = value.GetType();
            if (!type.IsValueType || type.IsPrimitive)
                throw new NotSupportedException();

            return ToByteArray(value);
        }

        private static byte[] ToByteArray(object @struct)
        {
            var size = Marshal.SizeOf(@struct.GetType());
            var data = new byte[size];
            var ptr = Marshal.AllocHGlobal(size);

            Marshal.StructureToPtr(@struct, ptr, fDeleteOld: true);
            Marshal.Copy(ptr, data, 0, size);
            Marshal.FreeHGlobal(ptr);

            return data;
        }

        private void MapProperty(PropertyInfo property, PropertyDefinition property_definition)
        {
            if (_options.EraseMember(property)) return;

            var type_definition = property_definition.DeclaringType;

            var getter = property.GetGetMethod(nonPublic: true);
            if (getter != null)
            {
                property_definition.GetMethod = type_definition.Methods.Single(m => m.Name == getter.Name);
                property_definition.GetMethod.IsGetter = true;
            }

            var setter = property.GetSetMethod(nonPublic: true);
            if (setter != null)
            {
                property_definition.SetMethod = type_definition.Methods.Single(m => m.Name == setter.Name);
                property_definition.SetMethod.IsSetter = true;
            }

            MapCustomAttributes(property, property_definition);
        }

        private PropertyDefinition PropertyDefinitionFor(PropertyInfo property, TypeDefinition declaringType)
        {
            var property_definition = new PropertyDefinition(
                property.Name,
                (MC.PropertyAttributes)property.Attributes,
                CreateReference(property.PropertyType, declaringType));

            declaringType.Properties.Add(property_definition);

            return property_definition;
        }

        private void MapEvent(EventInfo evt, EventDefinition event_definition)
        {
            if (_options.EraseMember(evt)) return;

            var type_definition = event_definition.DeclaringType;

            var add = evt.GetAddMethod(nonPublic: true);
            if (add != null)
            {
                event_definition.AddMethod = type_definition.Methods.Single(m => m.Name == add.Name);
                event_definition.AddMethod.IsAddOn = true;
            }

            var remove = evt.GetRemoveMethod(nonPublic: true);
            if (remove != null)
            {
                event_definition.RemoveMethod = type_definition.Methods.Single(m => m.Name == remove.Name);
                event_definition.RemoveMethod.IsRemoveOn = true;
            }

            var raise = evt.GetRaiseMethod(nonPublic: true);
            if (raise != null)
            {
                event_definition.InvokeMethod = type_definition.Methods.Single(m => m.Name == raise.Name);
                event_definition.InvokeMethod.IsFire = true;
            }

            MapCustomAttributes(evt, event_definition);
        }

        private EventDefinition EventDefinitionFor(EventInfo evt, TypeDefinition declaringType)
        {
            var event_definition = new EventDefinition(
                evt.Name,
                (MC.EventAttributes)evt.Attributes,
                CreateReference(evt.EventHandlerType, declaringType));

            declaringType.Events.Add(event_definition);

            return event_definition;
        }

        private void MapMethodBody(MethodBase method, MethodDefinition method_definition)
        {
            MapVariables(method, method_definition);
            MapInstructions(method, method_definition);
            MapExceptions(method, method_definition);
        }

        private void MapInstructions(MethodBase method, MethodDefinition method_definition)
        {
            var instructions = method.GetInstructions();

            foreach (var instruction in instructions)
            {
                var il = method_definition.Body.GetILProcessor();

                var op = OpCodeFor(instruction);

                switch (op.OperandType)
                {
                    case OperandType.InlineNone:
                        il.Emit(op);
                        break;
                    case OperandType.InlineMethod:
                        il.Emit(op, CreateReference((MethodBase)instruction.Operand, method_definition));
                        break;
                    case OperandType.InlineField:
                        il.Emit(op, CreateReference((FieldInfo)instruction.Operand, method_definition));
                        break;
                    case OperandType.InlineType:
                        il.Emit(op, CreateReference((Type)instruction.Operand, method_definition));
                        break;
                    case OperandType.InlineTok:
                        var member = (MemberInfo)instruction.Operand;
                        if (member is Type)
                            il.Emit(op, CreateTokenReference((Type)instruction.Operand, method_definition));
                        else if (member is FieldInfo)
                            il.Emit(op, CreateReference((FieldInfo)instruction.Operand, method_definition));
                        else if (member is MethodBase)
                            il.Emit(op, CreateTokenReference((MethodBase)instruction.Operand, method_definition));
                        else
                            throw new NotSupportedException();
                        break;
                    case OperandType.ShortInlineI:
                        if (op.Code == Code.Ldc_I4_S)
                            il.Emit(op, (sbyte)instruction.Operand);
                        else
                            il.Emit(op, (byte)instruction.Operand);
                        break;
                    case OperandType.InlineI:
                        il.Emit(op, (int)instruction.Operand);
                        break;
                    case OperandType.InlineI8:
                        il.Emit(op, (long)instruction.Operand);
                        break;
                    case OperandType.ShortInlineR:
                        il.Emit(op, (float)instruction.Operand);
                        break;
                    case OperandType.InlineR:
                        il.Emit(op, (double)instruction.Operand);
                        break;
                    case OperandType.ShortInlineVar:
                    case OperandType.InlineVar:
                        il.Emit(op, VariableFor(instruction, method_definition));
                        break;
                    case OperandType.ShortInlineArg:
                    case OperandType.InlineArg:
                        il.Emit(op, ParameterFor(instruction, method_definition));
                        break;
                    case OperandType.InlineString:
                        il.Emit(op, (string)instruction.Operand);
                        break;
                    case OperandType.ShortInlineBrTarget:
                    case OperandType.InlineBrTarget:
                        il.Emit(op, MC.Cil.Instruction.Create(OpCodes.Nop));
                        break;
                    case OperandType.InlineSwitch:
                        il.Emit(op, new[] { MC.Cil.Instruction.Create(OpCodes.Nop) });
                        break;
                    case OperandType.InlineSig:
                        throw new NotSupportedException("InlineSig");
                    default:
                        throw new NotSupportedException(op.OperandType.ToString());
                }
            }

            foreach (var instruction in instructions)
            {
                var op = OpCodeFor(instruction);

                switch (op.OperandType)
                {
                    case OperandType.ShortInlineBrTarget:
                    case OperandType.InlineBrTarget:
                        var br = OffsetToInstruction(instruction.Offset, instructions, method_definition);
                        var target = (MR.Instruction)instruction.Operand;
                        if (target != null)
                            br.Operand = OffsetToInstruction(target.Offset, instructions, method_definition);

                        break;

                    case OperandType.InlineSwitch:
                        var @switch = OffsetToInstruction(instruction.Offset, instructions, method_definition);
                        @switch.Operand = ((MR.Instruction[])instruction.Operand).Select(i => OffsetToInstruction(i.Offset, instructions, method_definition)).ToArray();
                        break;
                }
            }
        }

        private void MapVariables(MethodBase method, MethodDefinition method_definition)
        {
            var body = method.GetMethodBody();
            if (body == null)
                return;

            foreach (var variable in body.LocalVariables)
            {
                var variable_type = CreateReference(variable.LocalType, method_definition);
                method_definition.Body.Variables.Add(new VariableDefinition(variable.IsPinned ? new PinnedType(variable_type) : variable_type));
            }

            method_definition.Body.InitLocals = body.InitLocals;
        }

        private void MapExceptions(MethodBase method, MethodDefinition method_definition)
        {
            var body = method.GetMethodBody();
            if (body == null)
                return;

            var instructions = method.GetInstructions();

            foreach (var clause in body.ExceptionHandlingClauses)
            {
                var handler = new ExceptionHandler((ExceptionHandlerType)clause.Flags)
                {
                    TryStart = OffsetToInstruction(clause.TryOffset, instructions, method_definition),
                    TryEnd = OffsetToInstruction(clause.TryOffset + clause.TryLength, instructions, method_definition),
                    HandlerStart = OffsetToInstruction(clause.HandlerOffset, instructions, method_definition),
                    HandlerEnd = OffsetToInstruction(clause.HandlerOffset + clause.HandlerLength, instructions, method_definition)
                };

                switch (handler.HandlerType)
                {
                    case ExceptionHandlerType.Catch:
                        handler.CatchType = CreateReference(clause.CatchType, method_definition);
                        break;
                    case ExceptionHandlerType.Filter:
                        handler.FilterStart = OffsetToInstruction(clause.FilterOffset, instructions, method_definition);
                        break;
                }

                method_definition.Body.ExceptionHandlers.Add(handler);
            }
        }

        private static MC.Cil.Instruction OffsetToInstruction(int offset, IList<MR.Instruction> instructions, MethodDefinition method_definition)
        {
            var instruction = instructions.FirstOrDefault(i => i.Offset == offset);
            if (instruction == null)
                return null;

            return method_definition.Body.Instructions[instructions.IndexOf(instruction)];
        }

        private static AssemblyDefinition AssemblyDefinitionFor(Assembly assembly)
        {
            var name = assembly.GetName();

            var moduleParams = new ModuleParameters
            {
                Kind = ModuleKind.Dll,
                AssemblyResolver = new AppDomainAssemblyResolver()
            };

            var assembly_definition = AssemblyDefinition.CreateAssembly(
                new AssemblyNameDefinition(name.Name, name.Version),
                assembly.ManifestModule.Name, moduleParams);

            assembly_definition.MainModule.Runtime = TargetRuntime.Net_4_0;
            return assembly_definition;
        }

        private MethodDefinition MethodDefinitionFor(MethodBase method, TypeDefinition declaringType)
        {
            var method_definition = new MethodDefinition(
                method.Name,
                (MC.MethodAttributes)method.Attributes,
                _module_definition.TypeSystem.Void);

            method_definition.ImplAttributes = (MC.MethodImplAttributes)(int)method.GetMethodImplementationFlags();

            declaringType.Methods.Add(method_definition);

            var method_info = method as MethodInfo;

            if (method_info != null) {
                var generic_parameters = method_info.GetGenericArguments ();
                for (int i = 0; i < generic_parameters.Length; i++)
                    method_definition.GenericParameters.Add (GenericParameterFor (generic_parameters [i], method_definition));

                for (int i = 0; i < generic_parameters.Length; i++)
                    MapGenericParameterConstraints (generic_parameters [i], method_definition.GenericParameters [i], method_definition);
            }

            foreach (var parameter in method.GetParameters())
                MapParameter(method_definition, parameter);

            if (method_info != null)
                method_definition.ReturnType = CreateReference(method_info.ReturnType, method_definition);

            if (_options.MakePublic(method)) method_definition.IsPublic = true;

            return method_definition;
        }

        private void MapParameter(MethodDefinition method_definition, ParameterInfo parameter)
        {
            var parameter_definition = new ParameterDefinition(
                parameter.Name,
                (MC.ParameterAttributes)parameter.Attributes,
                CreateReference(parameter.ParameterType, method_definition));

            MapCustomAttributes(parameter, parameter_definition);

            method_definition.Parameters.Add(parameter_definition);
        }

        private FieldDefinition FieldDefinitionFor(FieldInfo field, TypeDefinition declaringType)
        {
            var field_definition = new FieldDefinition(
                field.Name,
                (MC.FieldAttributes)field.Attributes,
                CreateReference(field.FieldType, declaringType));

            declaringType.Fields.Add(field_definition);

            if (_options.MakePublic(field)) field_definition.IsPublic = true;

            return field_definition;
        }

        private TypeDefinition TypeDefinitionFor(Type type, TypeDefinition declaringType)
        {
            var type_definition = new TypeDefinition(
                type.IsNested ? "" : type.Namespace,
                type.Name,
                (MC.TypeAttributes)type.Attributes,
                _assembly_definition.MainModule.TypeSystem.Object);

            if (declaringType == null)
                _assembly_definition.MainModule.Types.Add(type_definition);
            else
                declaringType.NestedTypes.Add(type_definition);

            var generic_parameters = type.GetGenericArguments ();
            for (int i = 0; i < generic_parameters.Length; i++)
                type_definition.GenericParameters.Add (GenericParameterFor (generic_parameters[i], type_definition));

            for (int i = 0; i < generic_parameters.Length; i++)
                MapGenericParameterConstraints (generic_parameters [i], type_definition.GenericParameters [i], type_definition);

            type_definition.BaseType = type.BaseType != null
                ? CreateReference (type.BaseType, type_definition)
                : null;

            var layout = type.StructLayoutAttribute;

            if (layout != null && layout.Value != LayoutKind.Auto)
            {
                type_definition.PackingSize = (short)layout.Pack;
                type_definition.ClassSize = layout.Size;
            }

            if (_options.MakePublic(type))
                if (type_definition.IsNested)
                    type_definition.IsNestedPublic = true;
                else
                    type_definition.IsPublic = true;

            return type_definition;
        }

        private static GenericParameter GenericParameterFor(Type genericParameter, IGenericParameterProvider owner)
        {
            return new GenericParameter (genericParameter.Name, owner) {
                Attributes = (MC.GenericParameterAttributes) (int) genericParameter.GenericParameterAttributes
            };
        }

        private void MapGenericParameterConstraints (Type genericParameter, GenericParameter gp, IGenericParameterProvider owner)
        {
            foreach (var constraint in genericParameter.GetGenericParameterConstraints ()) {
                TypeReference reference =
                    (owner.GenericParameterType == GenericParameterType.Type)
                    ? CreateReference(constraint, (TypeReference)owner)
                    : CreateReference(constraint, (MethodReference)owner);

                gp.Constraints.Add(new GenericParameterConstraint(reference));
            }
        }

        private static ParameterDefinition ParameterFor(MR.Instruction instruction, MethodDefinition method)
        {
            var parameter = (ParameterInfo)instruction.Operand;
            return method.Parameters[parameter.Position];
        }

        private static VariableDefinition VariableFor(MR.Instruction instruction, MethodDefinition method)
        {
            var local = (LocalVariableInfo)instruction.Operand;
            return method.Body.Variables[local.LocalIndex];
        }

        private static readonly OpCode[] _opcodes = typeof(OpCodes)
            .GetFields(BindingFlags.Static | BindingFlags.Public)
            .Select(f => f.GetValue(null))
            .Cast<OpCode>()
            .ToArray();

        private static OpCode OpCodeFor(MR.Instruction instruction)
        {
            foreach (var opcode in _opcodes)
                if (opcode.Value == instruction.OpCode.Value)
                    return opcode;

            throw new NotSupportedException("OpCode not found: " + instruction.OpCode.Name);
        }

        private TypeReference CreateReference(Type type)
        {
            return MapReference(type, _module_definition.ImportReference(type));
        }

        private TypeReference CreateReference(Type type, TypeReference context)
        {
            return MapReference(type, _module_definition.ImportReference(type, context));
        }

        private TypeReference CreateReference(Type type, MethodReference context)
        {
            return MapReference(type, _module_definition.ImportReference(type, context));
        }

        private FieldReference CreateReference(FieldInfo field, MethodReference context)
        {
            var reference = _module_definition.ImportReference(field, context);
            MapReference(field.DeclaringType, reference.DeclaringType);
            MapReference(field.FieldType, reference.FieldType);
            return reference;
        }

        private MethodReference CreateReference(MethodBase method, MethodReference context)
        {
            var reference = _module_definition.ImportReference(method, context);
            MapReference(method.DeclaringType, reference.GetElementMethod().DeclaringType);
            MapReference(GetReturnType(method), reference.ReturnType);
            MapGenericArguments(method, reference);

            method.GetParameters().Zip(reference.Parameters, ((p, pr) => MapReference(p.ParameterType, pr.ParameterType))).ToList();

            return reference;
        }

        // Cecil seems to be doing a bad job at handling generic type definitions; 
        // this is a hack attempting to work around the problem.
        // related to F# 4.0 Quotation generation (see also https://github.com/mbraceproject/MBrace.StarterKit/issues/18)
        private TypeReference CreateTokenReference(Type type, MethodReference context)
        {
            var typeRef = CreateReference(type, context);
            if (type.IsGenericTypeDefinition)
            {
                return ((TypeSpecification)typeRef).ElementType;
            }
            else
            {
                return typeRef;
            }
        }

        private MethodReference CreateTokenReference(MethodBase method, MethodReference context)
        {
            var methRef = CreateReference(method, context);
            if (method.IsGenericMethodDefinition)
            {
                return ((MethodSpecification)methRef).ElementMethod;
            }
            else
            {
                return methRef;
            }
        }

        private void MapGenericArguments(MethodBase method, MethodReference reference)
        {
            var instance = reference as IGenericInstance;
            if (instance == null)
                return;

            method.GetGenericArguments().Zip(instance.GenericArguments, ((ga, gar) => MapReference(ga, gar))).ToList();
        }

        private void MapGenericArguments(Type type, TypeReference reference)
        {
            var instance = reference as IGenericInstance;
            if (instance == null)
                return;

            type.GetGenericArguments().Zip(instance.GenericArguments, ((ga, gar) => MapReference(ga, gar))).ToList();
        }

        // use correct dimension signature when referencing higher-rank arrays
        private void MapReference(ArrayType arrayType)
        {
            if (arrayType != null && arrayType.Rank > 1)
            {
                var rk = arrayType.Rank;
                arrayType.Dimensions.Clear();
                for (int i = 0; i < rk; i++)
                    arrayType.Dimensions.Add(new ArrayDimension(0, null));
            }
        }

        private TypeReference MapReference(Type t, TypeReference type)
        {
            if (type.IsGenericParameter)
                return type;

            if (type.IsPointer || type.IsByReference || type.IsPinned || type.IsArray)
            {
                MapReference(t.GetElementType(), ((TypeSpecification)type).ElementType);
                MapReference(type as ArrayType);

                return type;
            }

            if (type.IsGenericInstance)
            {
                MapGenericArguments(t, type);
                MapReference(t.GetGenericTypeDefinition(), ((TypeSpecification)type).ElementType);
                return type;
            }

            if (_options.RemapReference(t, out t))
            {
                var oldscope = type.Scope as AssemblyNameReference;
                if (oldscope != null && oldscope.FullName == _assembly_definition.FullName)
                    _module_definition.AssemblyReferences.Remove(oldscope);

                var _type = _module_definition.ImportReference(t);

                type.Name = _type.Name;
                type.Namespace = _type.Namespace;
                type.MetadataToken = _type.MetadataToken;
                type.GetElementType().Scope = _type.Scope;
            }

            if (type.Scope.MetadataScopeType != MetadataScopeType.AssemblyNameReference)
                return type;

            var reference = (AssemblyNameReference)type.Scope;
            if (reference.FullName != _assembly_definition.FullName)
                return type;

            type.GetElementType().Scope = _module_definition;
            _module_definition.AssemblyReferences.Remove(reference);

            return type;
        }

        private Type GetReturnType(MethodBase method)
        {
            var methodInfo = method as MethodInfo;
            if (methodInfo == null)
            {
                return method.DeclaringType;
            }

            return methodInfo.ReturnType;
        }

        private void MapCustomAttributes(SR.ICustomAttributeProvider provider, MC.ICustomAttributeProvider targetProvider)
        {
            var type = provider.GetType();

            // System.Reflection.Module.GetCustomAttributesData() not implemented in mono <= 3.4.0
            if (_is_running_mono && typeof(System.Reflection.Module).IsAssignableFrom(type)) return;

            var method = type.GetMethod("GetCustomAttributesData");
            if (method == null)
                throw new NotSupportedException("No method GetCustomAttributesData for type " + provider.GetType().FullName);

            var custom_attributes_data = (IList<CustomAttributeData>)method.Invoke(provider, new object[0]);

            foreach (var custom_attribute_data in custom_attributes_data)
            {
                var custom_attribute = new CustomAttribute(CreateReference(custom_attribute_data.Constructor, null));

                foreach (var argument in custom_attribute_data.ConstructorArguments)
                {
                    custom_attribute.ConstructorArguments.Add(CustomAttributeArgumentFor(argument));
                }

                foreach (var named_argument in custom_attribute_data.NamedArguments)
                {
                    var argument = new MC.CustomAttributeNamedArgument(named_argument.MemberInfo.Name, CustomAttributeArgumentFor(named_argument.TypedValue));
                    if (named_argument.MemberInfo is PropertyInfo)
                        custom_attribute.Properties.Add(argument);
                    else if (named_argument.MemberInfo is FieldInfo)
                        custom_attribute.Fields.Add(argument);
                    else
                        throw new NotSupportedException();
                }

                targetProvider.CustomAttributes.Add(custom_attribute);
            }
        }

        private CustomAttributeArgument CustomAttributeArgumentFor(CustomAttributeTypedArgument argument)
        {
            return new CustomAttributeArgument(
                CreateReference(argument.ArgumentType),
                MapCustomAttributeValue(argument));
        }

        private object MapCustomAttributeValue(CustomAttributeTypedArgument argument)
        {
            var value = argument.Value;
            var type = value as Type;
            if (type != null)
                return CreateReference(type);

            return value;
        }
    }
}
