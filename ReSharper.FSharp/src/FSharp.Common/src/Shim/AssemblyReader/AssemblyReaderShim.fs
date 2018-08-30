namespace rec JetBrains.ReSharper.Plugins.FSharp.Common.Shim.AssemblyReader

open System.Collections.Concurrent
open JetBrains.Application.changes
open JetBrains.DataFlow
open JetBrains.ProjectModel
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Common.Shim.AssemblyReader.ModuleReader
open JetBrains.ReSharper.Plugins.FSharp.Common.Shim.FileSystem
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel
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
         assemblyTimestampCache: AssemblyTimestampCache) =
    inherit AssemblyReaderShimBase(lifetime, changeManager)

    let assemblyReaders = ConcurrentDictionary<FileSystemPath, ReferencedAssembly>()

    let isSupported (project: IProject) =
        isNotNull project &&
        project.ProjectProperties.DefaultLanguage != FSharpProjectLanguage.Instance &&

        match project.ProjectProperties.ProjectKind with
        | ProjectKind.REGULAR_PROJECT
        | ProjectKind.WEB_SITE -> true
        | _ -> false

    let isAssembly (path: FileSystemPath) =
        let extension = path.ExtensionNoDot
        equalsIgnoreCase "dll" extension || equalsIgnoreCase "exe" extension

    let createReader (path: FileSystemPath) =
        use readLockCookie = ReadLockCookie.Create()
        let project = psiModules.GetProjectByOutputAssembly(path)
        if not (isSupported project) then Ignored else

        let psiModule =
            // review: can there be multiple project psi modules corresponding to a single output path?
            project.GetPsiModules() |> Seq.tryFind (fun psiModule ->
                match project.GetOutputAssemblyInfo(psiModule.TargetFrameworkId) with
                | null -> false
                | outputAssemblyInfo -> outputAssemblyInfo.Location = path)

        match psiModule with
        | None -> Ignored
        | Some psiModule -> ProjectOutput (new ModuleReader(psiModule, cache))

    let getReader path =
        let mutable reader = Unchecked.defaultof<_>
        match assemblyReaders.TryGetValue(path, &reader) with
        | true -> reader
        | _ ->

        let reader = createReader path
        assemblyReaders.[path] <- reader
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
