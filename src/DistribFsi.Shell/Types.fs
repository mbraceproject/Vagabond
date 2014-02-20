module internal Nessos.DistribFsi.Shell.Types

    open Nessos.DistribFsi

    open System
    open System.Reflection

    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Tast

    type InteractionCompilerMessage =
        | NextInteraction of ParsedInput list * TypedAssembly * string list * AsyncReplyChannel<exn option>
        | RequestCompilation of (* compileUnevaluatedInteraction *) bool * AsyncReplyChannel<Choice<FsiCompiledAssembly, exn>>
        | GetCompilerState of AsyncReplyChannel<InteractionCompilerState>

    and PendingFsiInteraction =
        {
            Name : string
            Declarations : Map<DeclarationId, FsiDeclarationInfo>
            ReferencedAssemblies : string list
            Ast : ParsedInput list
            TypeIndex : Map<DeclarationId, SynType>
        }

    and InteractionCompilerState =
        {
            // Accumulated Interaction Compiler Metadata
            Interactions : Map<string, FsiInteractionInfo> // interaction name -> interaction info
            CompiledAssemblies : Map<string, FsiCompiledAssembly> // assembly name -> assembly
            LastCompiledAssembly : FsiCompiledAssembly option

            // Pending Compilation State
            NextCompiledAssembly : AssemblyName
            Dependencies : Map<string, Assembly> // referenced assembly map: location -> assembly
            OpenModuleDecls : SynModuleDecl list // threaded 'open' declarations
            AstQueue : ParsedInput list
            InteractionQueue : FsiInteractionInfo list

            // Data related to pending interaction, if present
            PendingInteraction : PendingFsiInteraction option
        }
    with
        static member Empty =
            {
                Interactions = Map.empty
                CompiledAssemblies = Map.empty
                LastCompiledAssembly = None

                NextCompiledAssembly = null
                OpenModuleDecls = []
                Dependencies = Map.empty
                AstQueue = []
                InteractionQueue = []

                PendingInteraction = None
            }