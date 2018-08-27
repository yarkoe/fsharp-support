namespace rec JetBrains.ReSharper.Plugins.FSharp.Common.Shim.AssemblyReader

open System
open System.Collections.Concurrent
open System.Collections.Generic
open JetBrains.Application.changes
open JetBrains.DataFlow
open JetBrains.Metadata.Utils
open JetBrains.ProjectModel
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Common.Shim.FileSystem
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel
open JetBrains.ReSharper.Psi.Modules
open JetBrains.Threading
open JetBrains.Util.DataStructures
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader

type ReferencedAssembly =
    /// An output of a psi source project except for F# projects.
    | ProjectOutput of ModuleReader

    /// Not supported or F# project.
    | Ignored


/// Overrides default FCS assemblies reader to use the symbol cache for known non-F# projects.
[<SolutionComponent>]
type AssemblyReaderShim
        (lifetime: Lifetime, changeManager: ChangeManager, psiModules: IPsiModules, assemblyRefs: AssemblyRefs,
         assemblyTimestampCache: AssemblyTimestampCache) =
    inherit AssemblyReaderShimBase(lifetime, changeManager)

    let assemblyReaders = Dictionary<FileSystemPath, ReferencedAssembly>()
    let locker = JetFastSemiReenterableRWLock()

    /// An arbitrary timestamp that is earlier than any file modifications observed by FCS.
    let defaultTimestamp = DateTime.UtcNow

    let isSupported (project: IProject) =
        project :? ProjectImpl &&
        project.ProjectProperties.DefaultLanguage != FSharpProjectLanguage.Instance &&

        match project.ProjectProperties.ProjectKind with
        | ProjectKind.REGULAR_PROJECT
        | ProjectKind.WEB_SITE -> true
        | _ -> false

    let tryGetReader path =
        use lock = locker.UsingReadLock()
        assemblyReaders.TryGetValue(path) // todo: remove tuple allocation

    let addReader path reader =
        assemblyReaders.[path] <- reader
        reader

    let getReader path =
        match tryGetReader path with
        | true, reader -> reader
        | _ ->

        use lock = locker.UsingWriteLock()
        match assemblyReaders.TryGetValue(path) with
        | true, reader -> reader
        | _ ->

        let project = psiModules.GetProjectByOutputAssembly(path)
        if not (isSupported project) then addReader path Ignored else

        let project = project :?> ProjectImpl
        let reader = new ModuleReader(defaultTimestamp) // todo
        addReader path (ProjectOutput reader)

    override x.GetLastWriteTime(path) =
        match getReader path with
        | ProjectOutput reader -> reader.TimeStamp
        | _ -> base.GetLastWriteTime(path)

    override x.Exists(path) =
        match getReader path with
        | ProjectOutput _ -> true
        | _ -> base.Exists(path)

    override x.GetILModuleReader(filename, readerOptions) =
        base.GetILModuleReader(filename, readerOptions)

    override x.Execute(args) =
        let changeMap = args.ChangeMap
        let change = changeMap.GetChange<PsiModuleChange>(psiModules)
        let c' = change
        ()


type ModuleReader(timestamp: DateTime) =
    member val TimeStamp = timestamp with get, set

    interface ILModuleReader with
        member x.ILModuleDef = Unchecked.defaultof<_> // todo
        member x.ILAssemblyRefs = []
        member x.Dispose() = ()


[<SolutionComponent>]
type AssemblyRefs() =
    let assemblyRefs = ConcurrentDictionary<AssemblyNameInfo, ILAssemblyRef>()
    let cultures = DataIntern()

    member x.GetAssemblyRef(assemblyNameInfo: AssemblyNameInfo): ILAssemblyRef =
        match assemblyRefs.TryGetValue(assemblyNameInfo) with
        | true, ref -> ref
        | _ ->

        let name = assemblyNameInfo.Name
        let retargetable = assemblyNameInfo.IsRetargetable

        let publicKey =
            match assemblyNameInfo.GetPublicKeyToken2().GetArrayOrNull() with
            | null ->
                match assemblyNameInfo.GetPublicKey() with
                | null -> None
                | key -> Some (PublicKey.PublicKey key)
            | bytes -> Some (PublicKey.PublicKeyToken bytes)

        let version =
            match assemblyNameInfo.Version with
            | null -> None
            | v -> Some (ILVersionInfo (uint16 v.Major, uint16 v.Minor, uint16 v.Revision, uint16 v.Build))

        let locale =
            match assemblyNameInfo.Culture with
            | null -> None
            | culture -> cultures.Intern(Some culture)

        let hash = None // todo: what is it?
        let ref = ILAssemblyRef.Create(name, hash, publicKey, retargetable, version, locale)
        assemblyRefs.[assemblyNameInfo] <- ref
        ref
