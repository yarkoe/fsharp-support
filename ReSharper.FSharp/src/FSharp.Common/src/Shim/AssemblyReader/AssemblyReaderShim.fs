namespace rec JetBrains.ReSharper.Plugins.FSharp.Common.Shim.AssemblyReader

open System
open System.Collections.Concurrent
open JetBrains.Application.changes
open JetBrains.DataFlow
open JetBrains.ProjectModel
open JetBrains.ProjectModel.Properties
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Common.Shim.AssemblyReader.ModuleReader
open JetBrains.ReSharper.Plugins.FSharp.Common.Shim.FileSystem
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel
open JetBrains.ReSharper.Psi.Caches
open JetBrains.ReSharper.Psi.ExtensionsAPI.Caches2
open JetBrains.ReSharper.Psi.Modules
open JetBrains.ReSharper.Resources.Shell

type ReferencedAssembly =
    /// An output of a psi source project except for F# projects.
    | ProjectOutput of ModuleReader

    /// Not supported file or output assembly for F# project.
    | Ignored


/// Overrides default FCS assemblies reader to use ReSharper symbol cache for known non-F# projects.
[<SolutionComponent>]
type AssemblyReaderShim
        (lifetime: Lifetime, changeManager: ChangeManager, psiModules: IPsiModules, cache: ModuleReaderCache,
         projectsByOutput: IProjectsByOutput, assemblyTimestampCache: AssemblyTimestampCache) =
    inherit AssemblyReaderShimBase(lifetime, changeManager)

    // todo: remove on project/psi model changes
    let assemblyReadersByPath = ConcurrentDictionary<FileSystemPath, ReferencedAssembly>()
    let assemblyReadersByModule = ConcurrentDictionary<IPsiModule, ReferencedAssembly>()

    let isAssembly (path: FileSystemPath) =
        let extension = path.ExtensionNoDot
        equalsIgnoreCase "dll" extension || equalsIgnoreCase "exe" extension

    let createReader (path: FileSystemPath) =
        use readLockCookie = ReadLockCookie.Create()
        match projectsByOutput.GetProjectPsiModuleByOutputAssembly(path) with
        | null -> Ignored
        | psiModule -> ProjectOutput (new ModuleReader(psiModule, cache))

    let getReader path =
        let mutable reader = Unchecked.defaultof<_>
        match assemblyReadersByPath.TryGetValue(path, &reader) with
        | true -> reader
        | _ ->

        let reader = createReader path
        assemblyReadersByPath.[path] <- reader

        match reader with
        | ProjectOutput moduleReader ->
            assemblyReadersByModule.[moduleReader.Module] <- reader
        | _ -> ()

        reader

    override x.GetLastWriteTime(path) =
        if not (isAssembly path) then base.GetLastWriteTime(path) else

        match getReader path with
        | ProjectOutput reader -> reader.TimeStamp
        | _ -> base.GetLastWriteTime(path)

    override x.Exists(path) =
        if not (isAssembly path) then base.Exists(path) else

        match getReader path with
        | ProjectOutput _ -> true
        | _ -> base.Exists(path)

    override x.GetModuleReader(path, readerOptions) =
        match getReader path with
        | ProjectOutput reader -> reader :> _
        | _ -> base.GetModuleReader(path, readerOptions)

    member x.GetModuleReader(psiModule: IPsiModule): ReferencedAssembly =
        let mutable reader = Unchecked.defaultof<_>
        match assemblyReadersByModule.TryGetValue(psiModule, &reader) with
        | true -> reader
        | _ -> Ignored


/// Extracted interface for overriding in tests.
type IProjectsByOutput =
    abstract member GetProjectPsiModuleByOutputAssembly: path: FileSystemPath -> IPsiModule


[<SolutionComponent>]
type ProjectsByOutput(psiModules: IPsiModules) =
    let isSupported (project: IProject) =
        isNotNull project &&

        // F# cross-project reading is done by FCS.
        project.ProjectProperties.DefaultLanguage != FSharpProjectLanguage.Instance &&

        // We don't have symbol cache populated for C++ projects.
        project.ProjectProperties.DefaultLanguage != ProjectLanguage.CPP &&

        match project.ProjectProperties.ProjectKind with
        | ProjectKind.REGULAR_PROJECT
        | ProjectKind.WEB_SITE -> true
        | _ -> false

    interface IProjectsByOutput with
        member x.GetProjectPsiModuleByOutputAssembly(path) =
            let projectAndtargetFrameworkId = psiModules.GetProjectAndTargetFrameworkIdByOutputAssembly(path)
            if isNull (box projectAndtargetFrameworkId) then null else

            let project, targetFrameworkId = projectAndtargetFrameworkId 
            if not (isSupported project) then null else

            psiModules.GetPrimaryPsiModule(projectAndtargetFrameworkId)


[<SolutionComponent>]
type SymbolCacheListener(lifetime: Lifetime, symbolCache: ISymbolCache, readerShim: AssemblyReaderShim) =
    let typePartChanged = Action<_>(fun (typePart: TypePart) ->
        match readerShim.GetModuleReader(typePart.GetPsiModule()) with
        | ProjectOutput reader ->
            let clrTypeName = typePart.TypeElement.GetClrName()
            reader.InvalidateTypeDef(clrTypeName)
        | _ -> ())

    do
        symbolCache.add_OnAfterTypePartAdded(typePartChanged)
        symbolCache.add_OnBeforeTypePartRemoved(typePartChanged)

        lifetime.AddAction2(fun _ ->
            symbolCache.remove_OnAfterTypePartAdded(typePartChanged)
            symbolCache.remove_OnBeforeTypePartRemoved(typePartChanged))
