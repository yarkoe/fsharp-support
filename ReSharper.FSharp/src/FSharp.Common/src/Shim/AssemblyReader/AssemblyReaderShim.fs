namespace rec JetBrains.ReSharper.Plugins.FSharp.Common.Shim.AssemblyReader

open System
open System.Collections.Concurrent
open JetBrains.Application.changes
open JetBrains.DataFlow
open JetBrains.Metadata.Utils
open JetBrains.ProjectModel
open JetBrains.ProjectModel.Model2.Assemblies.Interfaces
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Common.Shim.FileSystem
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Modules
open JetBrains.ReSharper.Resources.Shell
open JetBrains.Metadata.Reader.API
open JetBrains.Util.DataStructures
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader

type ReferencedAssembly =
    /// An output of a psi source project except for F# projects.
    | ProjectOutput of ModuleReader

    /// Not supported file or output assembly for F# project.
    | Ignored


/// Overrides default FCS assemblies reader to use the symbol cache for known non-F# projects.
[<SolutionComponent>]
type AssemblyReaderShim
        (lifetime: Lifetime, changeManager: ChangeManager, psiModules: IPsiModules, typeRefs: TypeRefsCache,
         assemblyTimestampCache: AssemblyTimestampCache) =
    inherit AssemblyReaderShimBase(lifetime, changeManager)

    let assemblyReaders = ConcurrentDictionary<FileSystemPath, ReferencedAssembly>()

    let isSupported (project: IProject) =
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
            // review: can there be multiple project psi modules for one output path?
            project.GetPsiModules() |> Seq.tryFind (fun psiModule ->
                match project.GetOutputAssemblyInfo(psiModule.TargetFrameworkId) with
                | null -> false
                | outputAssemblyInfo -> outputAssemblyInfo.Location = path)

        match psiModule with
        | None -> Ignored
        | Some psiModule -> ProjectOutput (new ModuleReader(psiModule, typeRefs))

    let getReader path =
        match assemblyReaders.TryGetValue(path) with
        | true, reader -> reader
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

    override x.GetILModuleReader(filename, readerOptions) =
        base.GetILModuleReader(filename, readerOptions)

type ModuleReader(psiModule: IPsiModule, typeRefs: TypeRefsCache) =
    let psiServices = psiModule.GetPsiServices()
    let symbolScope = psiServices.Symbols.GetSymbolScope(psiModule, false, true)

    let literalTypes =
        [ PredefinedType.BOOLEAN_FQN, unbox >> ILFieldInit.Bool
          PredefinedType.STRING_FQN,  unbox >> ILFieldInit.String
          PredefinedType.CHAR_FQN,    unbox >> ILFieldInit.Char  
          PredefinedType.SBYTE_FQN,   unbox >> ILFieldInit.Int8  
          PredefinedType.BYTE_FQN,    unbox >> ILFieldInit.UInt8 
          PredefinedType.SHORT_FQN,   unbox >> ILFieldInit.Int16 
          PredefinedType.USHORT_FQN,  unbox >> ILFieldInit.UInt16
          PredefinedType.INT_FQN,     unbox >> ILFieldInit.Int32 
          PredefinedType.UINT_FQN,    unbox >> ILFieldInit.UInt32
          PredefinedType.LONG_FQN,    unbox >> ILFieldInit.Int64 
          PredefinedType.ULONG_FQN,   unbox >> ILFieldInit.UInt64
          PredefinedType.FLOAT_FQN,   unbox >> ILFieldInit.Single
          PredefinedType.DOUBLE_FQN,  unbox >> ILFieldInit.Double ] |> dict
    
    let mkLiteralValue (value: ConstantValue): ILFieldInit option =
        if value.IsBadValue() then None else
        if value.IsNull() then Some ILFieldInit.Null else
    
        match value.Type with
        | :? IDeclaredType as declaredType ->
            match literalTypes.TryGetValue(declaredType.GetClrName()) with
            | true, valueCtor -> Some (valueCtor value.Value)
            | _ -> None
        | _ -> None

    member val TimeStamp = DateTime.MinValue with get, set

    interface ILModuleReader with
        member x.ILModuleDef = Unchecked.defaultof<_> // todo
        member x.ILAssemblyRefs = []
        member x.Dispose() = ()


[<SolutionComponent>]
type TypeRefsCache(lifetime, changeManager) =
    inherit ChangeListenerBase(lifetime, changeManager)

    let assemblyScopes = ConcurrentDictionary<AssemblyNameInfo, ILScopeRef>()
    let localTypeRefs = ConcurrentDictionary<IClrTypeName, ILTypeRef>()
    let assemblyTypeRefs = ConcurrentDictionary<IPsiModule, ConcurrentDictionary<IClrTypeName, ILTypeRef>>()

    let publicKeys = DataIntern()
    let cultures = DataIntern()

    let neutralCulture = Some ""
    let digitStrings = [| "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" |]

    let createAssemblyScopeRef (assemblyName: AssemblyNameInfo): ILAssemblyRef =
        let name = assemblyName.Name
        let hash = None // review: assembly hash?
        let retargetable = assemblyName.IsRetargetable

        let publicKey =
            match assemblyName.GetPublicKeyToken2().GetArrayOrNull() with
            | null ->
                match assemblyName.GetPublicKey() with
                | null -> None
                | key -> publicKeys.Intern(Some (PublicKey.PublicKey key))
            | bytes -> publicKeys.Intern(Some (PublicKey.PublicKeyToken bytes))

        let version =
            match assemblyName.Version with
            | null -> None
            | v -> Some (ILVersionInfo (uint16 v.Major, uint16 v.Minor, uint16 v.Revision, uint16 v.Build))

        let locale =
            match assemblyName.Culture with
            | null -> None
            | "neutral" -> neutralCulture
            | culture -> cultures.Intern(Some culture)

        ILAssemblyRef.Create(name, hash, publicKey, retargetable, version, locale)

    let getAssemblyScope (assemblyName: AssemblyNameInfo): ILScopeRef =
        match assemblyScopes.TryGetValue(assemblyName) with
        | true, scopeRef -> scopeRef
        | _ ->

        let assemblyRef = ILScopeRef.Assembly (createAssemblyScopeRef assemblyName)
        assemblyScopes.[assemblyName] <- assemblyRef
        assemblyRef

    let mkILScopeRef (fromModule: IPsiModule) (targetModule: IPsiModule): ILScopeRef =
        if fromModule == targetModule then ILScopeRef.Local else
        let assemblyName =
            match targetModule.ContainingProjectModule with
            | :? IAssembly as assembly -> assembly.AssemblyName
            | :? IProject as project -> project.GetOutputAssemblyNameInfo(targetModule.TargetFrameworkId)
            | _ -> failwithf "mkIlScopeRef %O -> %O" fromModule targetModule
        getAssemblyScope assemblyName

    let mkTypeName (name: string) (paramsCount: int): string =
        if paramsCount = 0 then name else
        let paramsCountString =
            if paramsCount < 10 then digitStrings.[paramsCount] else paramsCount.ToString()
        String.Concat(name, "`", paramsCountString)

    let mkNameFromTypeNameAndParamsNumber (nameAndParametersCount: TypeNameAndTypeParameterNumber) =
        mkTypeName nameAndParametersCount.TypeName nameAndParametersCount.TypeParametersNumber

    let mkNameFromClrTypeName (clrTypeName: IClrTypeName) =
        mkTypeName clrTypeName.ShortName clrTypeName.TypeParametersCount

    member x.GetTypeRef(fromModule: IPsiModule, typeElement: ITypeElement) =
        let clrTypeName = typeElement.GetClrName()
        let targetModule = typeElement.Module

        let refCache =
            if fromModule == targetModule then localTypeRefs
            else assemblyTypeRefs.GetOrAdd(targetModule, fun _ -> ConcurrentDictionary())

        let mutable typeRef = Unchecked.defaultof<_>
        match refCache.TryGetValue(clrTypeName, &typeRef) with
        | true -> typeRef
        | _ ->

        let scopeRef = mkILScopeRef fromModule targetModule

        let typeRef =
            if fromModule != targetModule && localTypeRefs.TryGetValue(clrTypeName, &typeRef) then
                ILTypeRef.Create(scopeRef, typeRef.Enclosing, typeRef.Name) else
    
            let containingType = typeElement.GetContainingType()
    
            let enclosingTypes =
                match containingType with
                | null -> []
                | _ ->
    
                let enclosingTypeNames =
                    containingType.GetClrName().TypeNames
                    |> List.ofSeq
                    |> List.map mkNameFromTypeNameAndParamsNumber
    
                let ns = String.concat "." clrTypeName.NamespaceNames
                if StringUtil.IsEmpty(ns) then enclosingTypeNames else
    
                match enclosingTypeNames with
                | hd :: tl -> String.Concat(ns, ".", hd) :: tl
                | [] -> failwithf "mkTypeRef %O" clrTypeName
    
            let name =
                match containingType with
                | null -> clrTypeName.FullName
                | _ -> mkNameFromClrTypeName clrTypeName
    
            ILTypeRef.Create(scopeRef, enclosingTypes, name)

        refCache.[clrTypeName] <- typeRef
        typeRef

    override x.Execute(args) =
        for change in args.ChangeMap.GetChanges<PsiModuleChange>() do
            for change in change.ModuleChanges do
                match change.Type with
                | PsiModuleChange.ChangeType.Removed ->
                    assemblyTypeRefs.TryRemove(change.Item) |> ignore
                | _ -> ()
