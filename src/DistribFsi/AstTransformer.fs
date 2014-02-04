module internal Nessos.DistribFsi.AstTransformer

    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.Interactive
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.AbstractIL.IL

    open Microsoft.FSharp.Reflection

    open System
    open System.Collections
    open System.Reflection

    open Nessos.DistribFsi
    open Nessos.DistribFsi.Utils
    open Nessos.DistribFsi.Serialization

    let mkIdent (path : string list) =
        LongIdentWithDots(path |> List.map (fun e -> Ident(e, Range.range0)), [Range.range0])

    let getSynDeserializerExpr (fs : ISynSerializerProvider) =
        let ident = mkIdent fs.SynDeserializerPath
        SynExpr.LongIdent(false, ident, None, Range.range0)

    let containsReflectedDefinitionAttribute (binding : SynBinding) =
        let (Binding(_,_,_,_,attrs,_,_,_,_,_,_,_)) = binding
        
        let attrIsReflectedDefinition (attr : SynAttribute) =
            match attr.TypeName with
            | LongIdentWithDots(ident :: _, _) ->
                match ident.idText with
                | "ReflectedDefinition" | "Cloud" -> true
                | _ -> false
            | LongIdentWithDots _ -> false

        List.exists attrIsReflectedDefinition attrs

    let propertyContainsClosures (p : PropertyInfo) =
        let rec getNestedTypes(t : Type) =
            seq {
                for nt in t.GetNestedTypes(BindingFlags.NonPublic ||| BindingFlags.Public) do
                    yield nt
                    yield! getNestedTypes nt
            }

        let isClosureType (t : Type) = t.Name.StartsWith(p.Name + "@") //|| t.Name.StartsWith("clo@")

        p.DeclaringType
        |> getNestedTypes
        |> Seq.exists isClosureType

    type CurrentInteraction =
        {
            TypeIndex : Map<DeclarationId, SynType>
            DeclarationInfo : FsiDeclarationInfo list
            CompiledAssembly : FsiCompiledAssembly
            Dependencies : AssemblyDescriptor list
            ModuleNames : string list
            Inputs : ParsedInput list
        }

    let buildInteractionData (currentAssembly : FsiCompiledAssembly)
                                (tyIdx : Map<DeclarationId, SynType>)
                                (decls : FsiDeclarationInfo list) 
                                (inputs : ParsedInput list) =

        let referencedAssemblies =
            decls
            |> Seq.collect (fun d -> d.Dependencies)
            |> Seq.distinct
            |> Seq.choose (
                function
                | External (a,_) ->
                    Some <|
                        if a.IsDynamic then FsiTypeProvider a
                        else StaticAssembly a
                | Fsi _ -> None)
            |> Seq.distinct
            |> Seq.toList

        let moduleNames = decls |> Seq.map (fun d -> d.ModuleName) |> Seq.distinct |> Seq.toList

        {
            TypeIndex = tyIdx
            DeclarationInfo = decls
            CompiledAssembly = currentAssembly
            ModuleNames = moduleNames
            Dependencies = referencedAssemblies
            Inputs = inputs
        }


    let allFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static

    let mkCachedBinding(serializer : ISynSerializerProvider, orig : SynBinding, pat : SynPat, ty : SynType, bytes : byte []) =
        let bytes = SynExpr.Const(SynConst.Bytes(bytes, Range.range0), Range.range0)
        let tyApp = SynExpr.TypeApp(getSynDeserializerExpr serializer,Range.range0,[ty],[Range.range0],None,Range.range0, Range.range0)
        let expr = SynExpr.App(ExprAtomicFlag.NonAtomic, false, tyApp, bytes, Range.range0)
            
        match orig with
        | SynBinding.Binding(b1,b2,b3,b4,b5,b6,b7,_,b9,_,b11,b12) ->
            SynBinding.Binding(b1,b2,b3,b4,b5,b6,b7,pat,b9,expr,b11,b12)

    let mkNullBinding(orig : SynBinding, pat : SynPat, ty : SynType) =
        let ucdo = SynExpr.LongIdent(false, mkIdent ["Unchecked" ; "defaultof"], None, Range.range0)
        let expr = SynExpr.TypeApp(ucdo, Range.range0, [ty], [Range.range0], None, Range.range0, Range.range0)

        match orig with
        | SynBinding.Binding(b1,b2,b3,b4,b5,b6,b7,_,b9,_,b11,b12) ->
            SynBinding.Binding(b1,b2,b3,b4,b5,b6,b7,pat,b9,expr,b11,b12)

    let mkOpenDeclaration(path : string list) =
        SynModuleDecl.Open(mkIdent path, Range.range0)

    // extract value bindings from a given pattern
    let getValueBindings (pat : SynPat) =
        let gathered = ref List.empty<string * SynPat>

        let (|GetSynPats|) (args : SynConstructorArgs) =
            match args with
            | Pats ps -> ps
            | NamePatPairs (pairs,_) -> List.unzip pairs |> snd

        let rec gather depth =
            function
            | SynPat.Paren (np,_) -> gather (depth + 1) np
            | SynPat.Wild _ -> ()
            | SynPat.Named(_,ident,_,_,_) as pat -> gathered := (ident.idText, pat) :: !gathered
            // some value bindings are represented with LongIdents
            | SynPat.LongIdent(LongIdentWithDots ([ident],_),_,_,GetSynPats [],_,_) as pat -> gathered := (ident.idText, pat) :: !gathered
            // a top-level binding with args is always a function, ignore
            | SynPat.LongIdent _ when depth = 0 -> ()
            // nested longindent denotes a pattern match, arguments are bindings
            | SynPat.LongIdent(_,_,_,GetSynPats nps,_,_) -> List.iter (gather (depth + 1)) nps
            | SynPat.Typed (np,_,_) -> gather (depth + 1) np
            | SynPat.Tuple (nps,_)
            | SynPat.ArrayOrList (_,nps,_) -> List.iter (gather (depth + 1)) nps
            | SynPat.Record(nnps,_) -> List.iter (snd >> gather (depth + 1)) nnps
            | _ -> ()

        do gather 0 pat
        !gathered



    // performs the following transformations to the untyped AST
    // * adds top-level open declarations placed in previous interactions
    // * performs pickling of top-level value definitions, if possible

    let transformAst (fsiDynamicAssembly : Lazy<Assembly>)
                                (serializer : ISynSerializerProvider)
                                (preamble : SynModuleDecl list)
                                (info : CurrentInteraction) =
   
        let updatedDecls = ref []
        let preamble = ref preamble

        let longIdent2Path (lident : LongIdent) =
            lident |> Seq.map (fun i -> i.idText) |> String.concat "."

        let rec transformDecls (moduleOrNamespace : Choice<Type, LongIdent>) 
                                (topLevelModule : (string * bool) option) 
                                (decls : SynModuleDecls) =
            // handle "it" expressions
            let decls =
                match topLevelModule with
                | Some(_, (* isScript *) false) when decls.Length > 1 ->
                    let n = decls.Length
                    let first, last2 = List.partitioni (fun i _ -> i < n - 2) decls
                    let last =
                        match last2 with
                        | [SynModuleDecl.Let(l0,[Binding(b0,DoBinding,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)],l2) ;
                            SynModuleDecl.Let(_,[Binding(_,DoBinding,_,_,_,_,_,_,_,_,_,_)],_)] ->
                                [SynModuleDecl.Let(l0,[Binding(b0,NormalBinding,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)],l2)]
                        | _ -> last2
                    first @ last
                | _ -> decls

            // traverse recursive module hierarchies
            let decls = 
                match moduleOrNamespace with
                | Choice1Of2 t -> List.collect (transformModuleDecl t) decls
                | Choice2Of2 path -> List.map (transformNamespaceDecl path) decls

            // manage open declarations
            match topLevelModule with
            | Some (moduleName, isScript) ->
                let pre = !preamble
                // include an open declaration for this module
                preamble := pre @ [mkOpenDeclaration [moduleName]]
                // record open decls if interaction
                if not isScript then
                    let openDecls = decls |> List.filter (function SynModuleDecl.Open _ -> true | _ -> false)
                    preamble := !preamble @ openDecls

                pre @ decls

            | None -> decls

        and transformNamespaceDecl (ns : LongIdent) =
            function
            | SynModuleDecl.NestedModule(ComponentInfo(_,_,_,name,_,_,_,_) as m0,decls,m2,m3) as decl ->
                let name = longIdent2Path (ns @ name)
                let t = fsiDynamicAssembly.Value.GetType(name, true)
                let decls2 = transformDecls (Choice1Of2 t) None decls
                SynModuleDecl.NestedModule(m0,decls2,m2,m3)
            | decl -> decl

        and transformModuleDecl (bt : Type) =
            // recognize syntactic patterns that are free variables
            let (|PatternBindingWithFreeVars|_|) (b : SynBinding) =
                let (SynBinding.Binding(_,_,_,_,_,_,_,pat,_,_,_,_)) = b

                match getValueBindings pat with
                | [] -> None
                | xs -> Some xs

            // create polymorphic inline caching declarations for value bindings
            let createCachedBinding binding range (prop : PropertyInfo) (pattern : SynPat) =
                let id = DeclarationIdUtils.OfMemberInfo prop
                let synTy = info.TypeIndex.[id]
                let declInfo = info.DeclarationInfo |> List.find (fun d -> d.Id = id)
                let value = prop.GetValue(null)

                let binding2, declInfo =
                    if value = null || serializer.CheckSerializable (value.GetType()) then
                        try
                            let bytes = serializer.Serialize value
                            let binding2 = mkCachedBinding(serializer, binding, pattern, synTy, bytes)
                            binding2, { declInfo with DeclarationType = PickledProperty }
                        with _ ->
                            // serialization error, bind to Unchecked.defaultof<_> and mark as error
                            let binding2 = mkNullBinding(binding, pattern, synTy)
                            binding2, { declInfo with DeclarationType = DeletedProperty }
                    else
                        // nonserializable type, bind to Unchecked.defaultof<_> and mark as error
                        let binding2 = mkNullBinding(binding, pattern, synTy)
                        binding2, { declInfo with DeclarationType = DeletedProperty }

                SynModuleDecl.Let(false,[binding2],range), declInfo


            function
            // eliminate do exprs
            | SynModuleDecl.DoExpr _ -> []
            // handle pattern bindings
            | SynModuleDecl.Let(b, [PatternBindingWithFreeVars fvs as binding], r) as decl ->
                let names, patterns = List.unzip fvs
                let props = List.map (fun name -> bt.GetProperty(name, allFlags)) names

                if containsReflectedDefinitionAttribute binding 
                    || List.exists ((=) null) props
                    || List.exists propertyContainsClosures props then 
                    // a declaration that is reflected, is not a property
                    // or depends on local closures will not be erased
                    [decl]
                else
                    let decls, declInfo =
                        (props, patterns)
                        ||> List.zip
                        |>  List.map (fun (prop, pat) -> createCachedBinding binding r prop pat)
                        |>  List.unzip

                    updatedDecls := declInfo @ !updatedDecls
                    
                    decls
            // recursively traverse nested modules
            | SynModuleDecl.NestedModule(ComponentInfo(_,_,_,[name],_,_,_,_) as info, decls, b, r) ->
                let subtype = 
                    match bt.GetNestedType(name.idText, allFlags) with
                    | null ->
                        // account for possible ModuleSuffix compilation mapping attribute
                        bt.GetNestedType(name.idText + "Module", allFlags)
                    | t -> t
                let decls2 = transformDecls (Choice1Of2 subtype) None decls
                [ SynModuleDecl.NestedModule(info, decls2, b, r) ]
            | decl -> [ decl ]


        and transformModule =
            function
            | SynModuleOrNamespace(lident,isModule,defs,m1,m2,m3,m4) as m ->
                let moduleOrNamespace =
                    if isModule then
                        fsiDynamicAssembly.Value.GetType(longIdent2Path lident, true) |> Choice1Of2
                    else Choice2Of2 lident

                let isScript = lident.Length > 1
                let baseModule = lident.Head.idText
                let defs2 = transformDecls moduleOrNamespace (Some (baseModule,isScript)) defs
                SynModuleOrNamespace(lident,isModule,defs2,m1,m2,m3,m4)

        and transformInput =
            function
            | ParsedInput.ImplFile(ParsedImplFileInput (f0,f1,f2,f3,f4,modules,f5)) ->
                let modules = List.map transformModule modules
            
                ParsedInput.ImplFile(ParsedImplFileInput(f0,f1,f2,f3,f4,modules,f5))
            | x -> x

        let inputs = List.map transformInput info.Inputs

        updatedDecls.Value, preamble.Value, inputs

    // recursively traverses dependency graph
    let gatherDependencies (asmbIdx : Map<FsiCompiledAssembly, AssemblyDescriptor list>) (assemblies : AssemblyDescriptor seq) =

        let isIgnoredAssembly =
            function
            | StaticAssembly a -> 
                let matches (ts : Type []) a = ts |> Array.exists (fun t -> t.Assembly = a)
                a.GlobalAssemblyCache || matches [| typeof<int> ; typeof<int option> ;  typeof<ParsedInput> |] a
            | _ -> false

        let tryGetAssembly (a : AssemblyName) = Assembly.tryFind a.Name |> Option.map StaticAssembly

        let rec traverse (traversed : Set<AssemblyDescriptor>) (remaining : Set<AssemblyDescriptor>) =
            match remaining with
            | Set.Empty -> traversed |> Set.toList
            | Set.Cons(dep, rest) ->
                if isIgnoredAssembly dep || traversed.Contains dep then traverse traversed rest
                else
                    let newDependencies =
                        match dep with
                        | FsiCompiled a -> match asmbIdx.TryFind a with None -> Seq.empty | Some asmbs -> asmbs :> _ seq
                        | StaticAssembly a 
                        | FsiTypeProvider a -> 
                            a.GetReferencedAssemblies () |> Seq.choose tryGetAssembly
//                        |> Seq.filter (fun a -> not a.GlobalAssemblyCache)

                    traverse (traversed.Add dep) (Set.addMany rest newDependencies)

        traverse Set.empty (set assemblies)