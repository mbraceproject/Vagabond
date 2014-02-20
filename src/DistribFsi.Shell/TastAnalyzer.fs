module internal Nessos.DistribFsi.Shell.TastAnalyzer

    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Tast
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.AbstractIL.IL
    open Microsoft.FSharp.Compiler.Range

    open Nessos.DistribFsi
    open Nessos.DistribFsi.Shell.Utils
    open Nessos.DistribFsi.Shell.Types

    open System
    open System.Reflection

    let extractFsiMetadata (fsiDynamicAssembly : Assembly) (referencedAssemblies : string list) (ta : TypedAssembly) (ast : ParsedInput list) =
     
        let mkLongIdent (r : range) (path : string) =
            let idents =
                path.ToString().Split('`').[0].Split('.')
                |> Seq.map (fun tok -> Ident(tok, r))
                |> Seq.toList
            LongIdentWithDots(idents, [])
        
        let getDeclaringType (remoteAssemblyQualifiedName : string option) (declaringTypePath : string) =
            let assembly = 
                match remoteAssemblyQualifiedName with 
                | None -> fsiDynamicAssembly
                | Some aqn -> Assembly.Load aqn

            assembly.GetType(declaringTypePath, true)

        // rebuild SynType out of TType
        let rec ttypeToSynType (tt : TType) : SynType =
            match tt with
            | TType_tuple tts ->
                SynType.Tuple(tts |> List.map(fun tt -> (false, ttypeToSynType tt)), Range.range0)
            | TType_fun(dom, cod) ->
                SynType.Fun(ttypeToSynType dom, ttypeToSynType cod, Range.range0)
            | TType_ucase(ucr, _) -> ttypeToSynType(ucr.ReturnType)
            | TType_var typar -> typarToSynType typar
            | TType_measure mexpr -> measureExprToSynType mexpr
            | TType_forall(tars, body) ->
                let stars = List.map typarToSynType tars
                SynType.App(ttypeToSynType body, None, stars, [], None, false, Range.range0)
            | TType_app (tyconRef, args) ->
                let synArgs = List.map ttypeToSynType args
                match tyconRef.ToString() with
                | "Microsoft.FSharp.Core.[]`1" -> SynType.Array(1,synArgs.[0],Range.range0)
                | "Microsoft.FSharp.Core.[,]`1" -> SynType.Array(2,synArgs.[0],Range.range0)
                | "Microsoft.FSharp.Core.[,,]`1" -> SynType.Array(3,synArgs.[0],Range.range0)
                | "Microsoft.FSharp.Core.[,,,]`1" -> SynType.Array(4,synArgs.[0],Range.range0)
                | name ->
                    let synParam = SynType.LongIdent (mkLongIdent Range.range0 name)
                    SynType.App(synParam,None,synArgs,[],None,false,Range.range0)

        and measureExprToSynType (mexpr : MeasureExpr) =
            match mexpr with
            | MeasureVar v -> typarToSynType v
            | MeasureCon tr -> SynType.LongIdent(mkLongIdent Range.range0 <| tr.ToString())
            | MeasureProd (e1, e2) -> 
                SynType.Tuple([(false, measureExprToSynType e1) ; (false, measureExprToSynType e2)], Range.range0)
            | MeasureOne -> SynType.StaticConstant(SynConst.Int32 1, Range.range0)
            | MeasureInv e ->
                SynType.Tuple([(true, measureExprToSynType MeasureOne); (false, measureExprToSynType e)], Range.range0)

        and typarToSynType (typar : Typar) =
            match typar.Data.typar_solution with 
            | Some tt -> ttypeToSynType tt
            | None ->
                let star = SynTypar.Typar(typar.Data.typar_id, HeadTypeStaticReq, false)
                SynType.Var(star, Range.range0) 

        // Expr dependency traversal
        
        // gather dependencies from typed expressions
        let getDeclRefOfVal (value : Val) =
            seq {
                match value.ActualParent with
                | Parent typeRef ->
                    let path = typeRef.CompiledRepresentationForNamedType.BasicQualifiedName
                    let name = if value.IsCompilerGenerated then value.CompiledName else value.DisplayName
                    let id = DeclarationId.Create(path, value.LogicalName)
                    let assembly = if typeRef.IsLocalRef then None else Some (defaultArg typeRef.nlr.Ccu.QualifiedName typeRef.nlr.AssemblyName)
                    let dt = getDeclaringType assembly path
                    yield {
                        Id = id
                        DeclaringType = dt
                        IsFsiDeclaration = typeRef.IsLocalRef
                    }
                | ParentNone -> ()
            }

        // get dependency of foreign assembly
        let getDeclRefOfIL (ilMethod : ILMethodRef) =
            seq {
                match ilMethod.EnclosingTypeRef.Scope with
                | ILScopeRef.Assembly asmb -> 
                    let name = ilMethod.Name
                    let path = ilMethod.EnclosingTypeRef.FullName
                    let dt = getDeclaringType (Some asmb.QualifiedName) path
                    let id = DeclarationId.Create(path, name)
                    yield {
                        Id = id
                        DeclaringType = dt
                        IsFsiDeclaration = false
                    }
                | _ -> ()
            }

        // build out of entity
        let getDeclRefOfEntity (entity : Entity) =
            seq {
                let name = if entity.IsUnionTycon then None else Some entity.CompiledName
                let path = entity.CompiledRepresentationForNamedType.BasicQualifiedName
                let assemblyName = 
                    if entity.CompiledRepresentationForNamedType.Scope.IsLocalRef then None
                    else Some entity.CompiledRepresentationForNamedType.Scope.QualifiedName

                let id = DeclarationId.Create(path, ?name = name)
                let dt = getDeclaringType assemblyName path
                yield {
                    Id = id
                    DeclaringType = dt
                    IsFsiDeclaration = (dt.Assembly = fsiDynamicAssembly)
                }
            }


        // the traversal loop
        let rec gatherDeclsOfExpr (expr0 : Expr) : seq<FsiReference> =
            seq {
                match expr0 with
                | Expr.Const _ -> ()
                | Expr.Sequential (expr,expr',_,_,_) -> yield! gatherDeclsOfExprs [expr; expr']
                | Expr.Lambda (_,_,_,_,body,_,_) -> yield! gatherDeclsOfExpr body
                | Expr.TyLambda (_,_,body,_,_) -> yield! gatherDeclsOfExpr body
                | Expr.App (expr,_,_,exprs,_) -> yield! gatherDeclsOfExprs <| expr :: exprs
                | Expr.LetRec (bindings,expr,_,_) ->
                    let exprs = bindings |> List.map (function TBind(_,expr,_) -> expr)
                    yield! gatherDeclsOfExprs <| expr :: exprs
                | Expr.Let (TBind(_,expr,_),expr',_,_) -> 
                    yield! gatherDeclsOfExprs [expr ; expr']
                | Expr.Obj (_,_,_,expr,objExprMethods,interfaces,_) -> 
                    let exprs = 
                        interfaces |> List.map snd |> List.concat 
                        |> List.append objExprMethods
                        |> List.map (function TObjExprMethod(_,_,_,_,exprs,_) -> exprs)
                    yield! gatherDeclsOfExprs <| expr :: exprs
                | Expr.Match (_,_,decisionTree,decisionTreeTargets,_,_) ->
                    yield! gatherDeclsOfDecisionTree decisionTree
                    yield! decisionTreeTargets |> Seq.map (function TTarget(_,expr,_) -> expr) |> gatherDeclsOfExprs
                | Expr.StaticOptimization (_,expr,expr',_) -> yield! gatherDeclsOfExprs [expr; expr']
                | Expr.Op (tOp,_,exprs,_) -> 
                    yield! gatherDeclsOfExprs exprs
                    yield! gatherDeclsOfTOp tOp       
                | Expr.Quote (expr,foo,_,_,_) -> 
                    let exprs = match foo.Value with Some (_,exprs,_) -> exprs | _ -> []
                    yield! gatherDeclsOfExprs <| expr :: exprs
                | Expr.TyChoose (_,expr,_) -> yield! gatherDeclsOfExpr expr
                | Expr.Link exprRef -> yield! gatherDeclsOfExpr exprRef.Value
                | Expr.Val(valRef,_,_) -> yield! getDeclRefOfVal valRef.Deref
            }

        and gatherDeclsOfTOp (tOp : TOp) : seq<FsiReference> =
            seq {
                match tOp with 
                | TOp.LValueOp (_,valRef) -> yield! getDeclRefOfVal valRef.Deref
                | TOp.ILCall (_,_,_,_,_,_,_,ilmethod,_,_,_) -> yield! getDeclRefOfIL ilmethod
                | TOp.ExnConstr t
                | TOp.Recd(_,t)
                | TOp.ValFieldGet(RFRef(t,_))
                | TOp.ValFieldGetAddr(RFRef(t,_))
                | TOp.ValFieldSet(RFRef(t,_))
                | TOp.UnionCaseTagGet t
                | TOp.UnionCaseProof(UCRef(t,_))
                | TOp.UnionCaseFieldGet(UCRef(t,_),_)
                | TOp.UnionCaseFieldSet(UCRef(t,_),_)
                | TOp.ExnFieldGet(t,_)
                | TOp.ExnFieldSet(t,_)
                | TOp.UnionCase(UCRef(t,_)) -> yield! getDeclRefOfEntity t.Deref
                | _ -> ()
            }

        and gatherDeclsOfExprs (exprs : seq<Expr>) : seq<FsiReference> =
            seq { for expr in exprs do yield! gatherDeclsOfExpr expr }

        and gatherDeclsOfDecisionTree (dt : DecisionTree) : seq<FsiReference> =
            seq {
                match dt with
                | TDSuccess (exprs,_) -> yield! gatherDeclsOfExprs exprs  
                | TDBind (TBind(_,expr,_), dTree) -> yield! gatherDeclsOfExpr expr ; yield! gatherDeclsOfDecisionTree dTree
                | TDSwitch (expr, decisionTreeCases, dTree,_) ->
                    yield! gatherDeclsOfExpr expr

                    let tests, dts = decisionTreeCases |> List.map (function TCase(test,dt) -> test,dt) |> List.unzip
                    for dt in dts do yield! gatherDeclsOfDecisionTree dt
                    for test in tests do yield! gatherDeclsOfTest test

                    match dTree with | Some dTree -> yield! gatherDeclsOfDecisionTree dTree | _ -> ()
            }

        and gatherDeclsOfTest (test : Test) : seq<FsiReference> =
            seq {
                match test with
                | Test.ActivePatternCase (expr,_,foo,_,_) ->
                    yield! gatherDeclsOfExpr expr
                    match foo with Some(valRef,_) -> yield! getDeclRefOfVal valRef.Deref | _ -> ()
                | _ -> ()
            }

        // root binding traversal and analysis
        let rec analyzeTModules (mexpr : ModuleOrNamespaceExpr) =
            seq {
                match mexpr with
                | TMDefDo _ -> ()
                | TMDefLet(binding,_) -> yield! analyzeRootBindings [binding]
                | TMDefs mexprs -> for mexpr in mexprs do yield! analyzeTModules mexpr
                | TMAbstract(ModuleOrNamespaceExprWithSig(_,mexpr,_)) -> yield! analyzeTModules mexpr
                | TMDefRec(_,bindings,mexprs,_) ->
                    yield! analyzeRootBindings bindings

                    for mexpr in mexprs do
                        let (ModuleOrNamespaceBinding(_,mexpr)) = mexpr
                        yield! analyzeTModules mexpr
            }

        // analyzes bindings per module
        and analyzeRootBindings (bindings : Binding list) =
            let mkDeclaration = 
                function
                | TBind(var,expr,_) ->
                    match getDeclRefOfVal var |> Seq.tryPick Some with
                    | Some { IsFsiDeclaration = true ; Id = id ; DeclaringType = dt } ->
//                        let isMethod = match expr with Expr.Lambda _ | Expr.TyLambda _ -> true | _ -> false
                        let isClosure = declarationDefinesClosures dt id.Name
                        let memberInfo = dt.GetMember(id.Name, allFlags).[0]
                        let isReflectedDefn = containsReflectedDefinitionAttribute memberInfo
                        let synType = ttypeToSynType var.Type
                        let decl = 
                            { 
                                Id = id
                                MemberInfo = memberInfo
                                References = []
                                DefinesCompilerGeneratedTypes = isClosure
                                IsReflectedDefinition = isReflectedDefn
                                // the following properties are to be determined after evaluation
                                IsNonSerializableValue = false
                                IsPickledValue = false
                            }
                        Some((synType, decl), expr)
                    | Some _ | None -> None

            let decls, exprs = bindings |> List.choose mkDeclaration |> List.unzip
            let declIds = decls |> Seq.map (fun (_,d) -> d.Id) |> set

            // compute references for binding
            let references = 
                gatherDeclsOfExprs exprs
                |> Seq.distinct 
                // filter (mutual) recursive occurrences
                |> Seq.filter (function { Id = id } -> not <| declIds.Contains id)
                |> Seq.toList
            
            decls |> Seq.map (fun (ty,d) -> ty, { d with References = references })


        let (TAssembly implFiles) = ta
        // do not use the qualified name, as this contains the file name in loaded scripts
        let moduleName = implFiles |> Seq.pick (function TImplFile(qname,_,ModuleOrNamespaceExprWithSig(ty,_,_),_,_) -> Some ty.ModuleAndNamespaceDefinitions.Head.DisplayName)
        let declarations = 
            implFiles 
            |> Seq.map (function TImplFile(_,_,ModuleOrNamespaceExprWithSig(_,tmod,_),_,_) -> tmod)
            |> Seq.collect analyzeTModules
            |> Seq.toList

        let tyIdx = declarations |> Seq.map (fun (ty,decl) -> decl.Id, ty) |> Map.ofSeq
        let decls = declarations |> Seq.map (fun (_,decl) -> decl.Id, decl) |> Map.ofSeq

        {
            Name = moduleName
            Declarations = decls
            TypeIndex = tyIdx
            Ast = ast
            ReferencedAssemblies = referencedAssemblies
        }