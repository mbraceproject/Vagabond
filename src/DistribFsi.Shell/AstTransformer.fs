module internal Nessos.DistribFsi.Shell.AstTransformer

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
    open Nessos.DistribFsi.Shell.Utils
    open Nessos.DistribFsi.Shell.Types

    let mkIdent range (path : string list) =
        LongIdentWithDots(path |> List.map (fun e -> Ident(e, range)), [range])

    let deserializerMethod = lazy(
        match typeof<Nessos.DistribFsi.SerializationSupport>.GetMethod("UnPickle") with
        | null -> invalidOp "DistribFsi: Could not locate dserialization method."
        | m when m.IsStatic && not m.DeclaringType.IsGenericType && m.ReturnType = typeof<obj>
                    && m.GetParameters() |> Array.map (fun p -> p.ParameterType) = [|typeof<byte []>|] -> m
        | _ -> invalidOp "DistribFsi: Invalid deserialization method.")

    let deserializerAssembly = lazy deserializerMethod.Value.DeclaringType.Assembly
    let synDeserializerPath = lazy(
        [ 
            yield! deserializerMethod.Value.DeclaringType.FullName.Split([|'.' ; '+'|])
            yield deserializerMethod.Value.Name 
        ])

    let mkCachedBinding(orig : SynBinding, pat : SynPat, ty : SynType, bytes : byte []) =
        let range = orig.RangeOfBindingAndRhs
        // rebuild: let binding = deserializer bytes :?> ty
        let bytes = SynExpr.Const(SynConst.Bytes(bytes, range), range)
        let synDeserializer = SynExpr.LongIdent(false, mkIdent range synDeserializerPath.Value, None, range)
        let appl = SynExpr.App(ExprAtomicFlag.NonAtomic, false, synDeserializer, bytes, range)
        let expr = SynExpr.Downcast(appl, ty, range)
            
        match orig with
        | SynBinding.Binding(b1,b2,b3,b4,b5,b6,b7,_,b9,_,b11,b12) ->
            SynBinding.Binding(b1,b2,b3,b4,b5,b6,b7,pat,b9,expr,b11,b12)

    let mkNullBinding(orig : SynBinding, pat : SynPat, ty : SynType) =
        let range = orig.RangeOfBindingAndRhs
        let ucdo = SynExpr.LongIdent(false, mkIdent range ["Unchecked" ; "defaultof"], None, range)
        let expr = SynExpr.TypeApp(ucdo, range, [ty], [range], None, range, range)

        match orig with
        | SynBinding.Binding(b1,b2,b3,b4,b5,b6,b7,_,b9,_,b11,b12) ->
            SynBinding.Binding(b1,b2,b3,b4,b5,b6,b7,pat,b9,expr,b11,b12)

    let mkOpenDeclaration (range : range) (path : string list) =
        SynModuleDecl.Open(mkIdent range path, range)

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

    let transformAst (fsiDynamicAssembly : Assembly) (pickleValues : bool) (preamble : SynModuleDecl list) (interaction : PendingFsiInteraction) =

        let declarations = ref interaction.Declarations
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
                preamble := pre @ [mkOpenDeclaration Range.range0 [moduleName]]
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
                let t = fsiDynamicAssembly.GetType(name, true)
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
            let createCachedBinding binding range (pattern : SynPat) (declaration : FsiDeclarationInfo) =
                let id = declaration.Id
                let synTy = interaction.TypeIndex.[id]
                let value = (declaration.MemberInfo :?> PropertyInfo).GetValue(null)

                let binding =
                    try
                        let bytes = Nessos.DistribFsi.SerializationSupport.Pickle value
                        let binding = mkCachedBinding(binding, pattern, synTy, bytes)
                        // successful, update declaration entry
                        declarations := declarations.Value.Add(id, {declaration with IsPickledValue = true})
                        binding
                    with _ ->
                        // serialization error, leave intact
                        declarations := declarations.Value.Add(id, {declaration with IsNonSerializableValue = true})
                        binding
//                        // serialization error, bind to Unchecked.defaultof<_> and mark as error
//                        let binding2 = mkNullBinding(binding, pattern, synTy)
//                        binding2, { declInfo with DeclarationType = DeletedProperty }

                SynModuleDecl.Let(false,[binding],range)


            function
            // eliminate do exprs
            | SynModuleDecl.DoExpr _ -> []
            // handle pattern bindings
            | SynModuleDecl.Let(b, [PatternBindingWithFreeVars vars as binding], r) as decl when pickleValues ->
                let info = 
                    vars |> List.map(fun (name,pat) -> let id = DeclarationId.Create(bt.FullName, name) in pat, declarations.Value.[id])

                if info |> List.exists (fun (_,decl) -> decl.IsMethod || decl.IsReflectedDefinition || decl.DefinesCompilerGeneratedTypes) then
                    // a declaration that is reflected or depends on local closures will not be erased
                    [ decl ]
                else
                    info |>  List.map (fun (pat, decl) -> createCachedBinding binding r pat decl)

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
                        fsiDynamicAssembly.GetType(longIdent2Path lident, true) |> Choice1Of2
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

        let ast' = List.map transformInput interaction.Ast

        declarations.Value, preamble.Value, ast'



    // performs AST transformation on a pending interaction, 
    // preferably after its evaluation has completed in the dynamic assembly
    // Trees pushed to the AST queue and metadata updated

    let popPendingInteraction fsiDynamicAssembly pickleValues (state : InteractionCompilerState) =
        match state.PendingInteraction with
        | None -> state
        | Some pendingInteraction ->
            
            let declarations, openModuleDecls, ast = transformAst fsiDynamicAssembly pickleValues state.OpenModuleDecls pendingInteraction

            let rec loadAssemblies (map : Map<string, Assembly>) (locations : string list) =
                match locations with
                | [] -> map
                | l :: rest when map.ContainsKey l -> loadAssemblies map rest
                | l :: rest ->
                    let a = Assembly.ReflectionOnlyLoadFrom l
                    loadAssemblies (map.Add(l,a)) rest

            let dependencyIndex = loadAssemblies state.Dependencies pendingInteraction.ReferencedAssemblies

            let interaction =
                {
                    Name = pendingInteraction.Name
                    Declarations = declarations
                    ContainingAssembly = state.NextCompiledAssembly
                }

            { state with
                AstQueue = state.AstQueue @ ast
                InteractionQueue = state.InteractionQueue @ [interaction]
                Dependencies = dependencyIndex
                OpenModuleDecls = openModuleDecls
                Interactions = state.Interactions.Add(interaction.Name, interaction)
                PendingInteraction = None
            }
