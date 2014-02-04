module internal Nessos.DistribFsi.TastAnalyzer

    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Tast
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.AbstractIL.IL
    open Microsoft.FSharp.Compiler.Range

    open System
    open System.Reflection

    let mkLongIdent (r : range) (path : string) =
        let idents =
            path.ToString().Split('`').[0].Split('.')
            |> Seq.map (fun tok -> Ident(tok, r))
            |> Seq.toList
        LongIdentWithDots(idents, [])

    let getDeclarationId(name : string, path : string) =
        let paths = path.Split('`').[0].Split([|'.';'+'|])
        [ yield! paths ; yield name ]

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
                let id = getDeclarationId(name, path)
                if typeRef.IsLocalRef then
                    let moduleName = 
                        match typeRef.CompilationPath.AccessPath with [] -> typeRef.CompiledName | h :: _ -> fst h
                    yield Fsi (moduleName, id)
                else yield External (Assembly.Load (let ssx = defaultArg typeRef.nlr.Ccu.QualifiedName typeRef.nlr.AssemblyName in ssx), id)
            | ParentNone -> ()
        }

    // get dependency of foreign assembly
    let getDeclRefOfIL (ilMethod : ILMethodRef) =
        seq {
            let name = ilMethod.Name
            let path = ilMethod.EnclosingTypeRef.FullName
            match ilMethod.EnclosingTypeRef.Scope with
            | ILScopeRef.Assembly asmb -> 
                let id = getDeclarationId(name, path)
                let assembly = Assembly.Load asmb.QualifiedName
                yield External (assembly, id)
            | _ -> ()
        }

    // build out of entity
    let getDeclRefOfEntity (entity : Entity) =
        seq {
            let id = getDeclarationId(entity.LogicalName, entity.CompiledRepresentationForNamedType.BasicQualifiedName)
            let t = System.Type.GetType entity.CompiledRepresentationForNamedType.QualifiedName
            if t <> null then yield External (t.Assembly, id)
            else
                let moduleName = 
                    match entity.CompilationPath.AccessPath with [] -> entity.CompiledName | h :: _ -> fst h
                yield Fsi (moduleName, id)
        }


    // the traversal loop
    let rec gatherDeclsOfExpr (expr0 : Expr) : seq<Declaration> =
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

    and gatherDeclsOfTOp (tOp : TOp) =
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

    and gatherDeclsOfExprs (exprs : seq<Expr>) : seq<Declaration> =
        seq { for expr in exprs do yield! gatherDeclsOfExpr expr }

    and gatherDeclsOfDecisionTree (dt : DecisionTree) : seq<Declaration> =
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

    and gatherDeclsOfTest (test : Test) =
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
                | Some (Fsi (modl,id)) ->
                    let declType = match expr with Expr.Lambda _ | Expr.TyLambda _ -> Method | _ -> UnsafeProperty
                    let synType = ttypeToSynType var.Type
                    let decl = { Id = id; ModuleName = modl; DeclarationType = declType; Dependencies = []}
                    Some((synType, decl), expr)
                | Some (External _) | None -> None

        let decls, exprs = bindings |> List.choose mkDeclaration |> List.unzip
        let declIds = decls |> Seq.map (fun (_,d) -> d.Id) |> set

        // compute dependencies for binding
        let dependencies = 
            gatherDeclsOfExprs exprs
            |> Seq.distinct 
            // filter (mutual) recursive occurrences
            |> Seq.filter (function Fsi (_,id) -> not <| declIds.Contains id | _ -> true)
            |> Seq.toList
            
        decls |> Seq.map (fun (ty,d) -> ty, { d with Dependencies = dependencies })

        
    let extractFsiMetadata (ta : TypedAssembly) =
        let (TAssembly implFiles) = ta
        let tmodules = 
            implFiles |> List.map (function TImplFile(_,_,ModuleOrNamespaceExprWithSig(_,tmod,_),_,_) -> tmod)

        let decls = [ for tmod in tmodules do yield! analyzeTModules tmod ]

        let tyIdx = decls |> Seq.map (fun (ty,decl) -> decl.Id, ty) |> Map.ofSeq
        let decls = decls |> List.map snd

        tyIdx, decls