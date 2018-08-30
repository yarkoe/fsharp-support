module rec JetBrains.ReSharper.Plugins.FSharp.Common.Shim.AssemblyReader.ModuleReader

open System
open System.Linq
open JetBrains.Metadata.Reader.API
open JetBrains.Metadata.Utils
open System.Collections.Concurrent
open System.Collections.Generic
open System.Reflection
open JetBrains.ProjectModel
open JetBrains.ProjectModel.Model2.Assemblies.Interfaces
open JetBrains.ProjectModel.Properties.Managed
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Modules
open JetBrains.ReSharper.Psi.Resolve
open JetBrains.ReSharper.Resources.Shell
open JetBrains.Util
open JetBrains.Util.DataStructures
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader

type ModuleReader(psiModule: IPsiModule, cache: ModuleReaderCache) =
    let psiServices = psiModule.GetPsiServices()
    let symbolScope = psiServices.Symbols.GetSymbolScope(psiModule, false, true)

    let mutable cachedModuleDef: ILModuleDef option = None

    let rec mkType (fromModule: IPsiModule) (typ: IType): ILType =
        if typ.IsVoid() then ILType.Void else

        match typ with
        | :? IDeclaredType as dt ->
            match dt.Resolve() with
            | :? EmptyResolveResult ->
                // todo: add per-module singletons for predefines types
                mkType fromModule (fromModule.GetPredefinedType().Object)

            | resolveResult ->

            match resolveResult.DeclaredElement with
            | :? ITypeParameter as typeParameter ->
                // todo: is it suitable for methods type parameters?
                let mutable index = typeParameter.Index
                let mutable parent = typeParameter.OwnerType.GetContainingType()
                while isNotNull parent do
                    index <- index + parent.TypeParameters.Count
                    parent <- parent.GetContainingType()

                ILType.TypeVar (uint16 index)

            // todo: value types, etc
            | :? ITypeElement as typeElement ->
                let typeRef = cache.GetTypeRef(fromModule, typeElement)
                let typeArgs =
                    resolveResult.Substitution.Domain
                    |> Seq.map (fun typeParameter -> mkType fromModule resolveResult.Substitution.[typeParameter])
                    |> List.ofSeq
                let typeSpec = ILTypeSpec.Create(typeRef, typeArgs)
                ILType.Boxed typeSpec // todo: intern

            | _ -> failwithf "mkType: %O" typ

        | :? IArrayType as arrayType ->
            let elementType = mkType fromModule arrayType.ElementType
            let shape = ILArrayShape.FromRank(arrayType.Rank) // todo: add tests for different dimensions
            ILType.Array (shape, elementType)

        | :? IPointerType as pointerType ->
            let elementType = mkType fromModule pointerType.ElementType
            ILType.Ptr elementType // todo: intern

        | _ -> failwithf "mkType: %O" typ

    let mkGenericVariance (variance: TypeParameterVariance): ILGenericVariance =
        match variance with
        | TypeParameterVariance.INVARIANT -> ILGenericVariance.NonVariant
        | TypeParameterVariance.IN -> ILGenericVariance.ContraVariant
        | TypeParameterVariance.OUT -> ILGenericVariance.CoVariant
        | _ -> failwithf "mkGenericVariance: %O" variance

    let mkGenericParameterDef (fromModule: IPsiModule) (typeParameter: ITypeParameter): ILGenericParameterDef =
        let typeConstraints =
            typeParameter.TypeConstraints
            |> List.ofSeq
            |> List.map (mkType fromModule)

        let attributes = storeILCustomAttrs emptyILCustomAttrs // todo

        { Name = typeParameter.ShortName
          Constraints = typeConstraints
          Variance = mkGenericVariance typeParameter.Variance
          HasReferenceTypeConstraint = typeParameter.IsClassType
          HasNotNullableValueTypeConstraint = typeParameter.IsValueType
          HasDefaultConstructorConstraint = typeParameter.HasDefaultConstructor
          CustomAttrsStored = attributes
          MetadataIndex = NoMetadataIdx }

    member x.Module = psiModule
    member x.SymbolScope = symbolScope

    // Initial value is an arbitrary timestamp that is earlier than any file modifications observed by FCS.
    member val TimeStamp = DateTime.MinValue with get, set

    interface ILModuleReader with
        member x.ILModuleDef =
            match cachedModuleDef with
            | Some moduleDef -> moduleDef
            | None ->

            use readLockCookie = ReadLockCookie.Create()

            // todo: change when reading assemblies as well
            let project = psiModule.ContainingProjectModule :?> IProject
            let moduleName = project.Name
            let assemblyName = project.GetOutputAssemblyName(psiModule.TargetFrameworkId)
            let isDll = isDll project

            let typeDefs =
                let result = List<ILPreTypeDef>()
                let rec addTypes (ns: INamespace) =
                    for typeElement in ns.GetNestedTypeElements(symbolScope) do
                        let clrTypeName = typeElement.GetClrName().GetPersistent() // todo: intern
                        result.Add(PreTypeDef(clrTypeName, x))
                    for nestedNs in ns.GetNestedNamespaces(symbolScope) do
                        addTypes nestedNs
                addTypes symbolScope.GlobalNamespace

                let preTypeDefs = result.ToArray()
                mkILTypeDefsComputed (fun _ -> preTypeDefs)

            let flags = 0 // todo
            let exportedTypes = mkILExportedTypes []

            let moduleDef =
                mkILSimpleModule
                    assemblyName moduleName isDll
                    DummyModuleDefValues.subsystemVersion
                    DummyModuleDefValues.useHighEntropyVA
                    typeDefs
                    None None flags exportedTypes
                    DummyModuleDefValues.metadataVersion

            cachedModuleDef <- Some moduleDef
            moduleDef

        member x.ILAssemblyRefs = []
        member x.Dispose() = ()


/// A common cache shared by all module readers.
[<SolutionComponent>]
type ModuleReaderCache(lifetime, changeManager) =
    inherit ChangeListenerBase(lifetime, changeManager)

    let assemblyRefs = ConcurrentDictionary<AssemblyNameInfo, ILScopeRef>()
    let localTypeRefs = ConcurrentDictionary<IClrTypeName, ILTypeRef>()
    let assemblyTypeRefs = ConcurrentDictionary<IPsiModule, ConcurrentDictionary<IClrTypeName, ILTypeRef>>()

    let cultures = DataIntern()
    let publicKeys = DataIntern()
    let literalValues = DataIntern()

    let neutralCulture = Some ""

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
        let mutable scopeRef = Unchecked.defaultof<_>
        match assemblyRefs.TryGetValue(assemblyName, &scopeRef) with
        | true -> scopeRef
        | _ ->

        let assemblyRef = ILScopeRef.Assembly (createAssemblyScopeRef assemblyName)
        assemblyRefs.[assemblyName] <- assemblyRef
        assemblyRef

    let mkILScopeRef (fromModule: IPsiModule) (targetModule: IPsiModule): ILScopeRef =
        if fromModule == targetModule then ILScopeRef.Local else
        let assemblyName =
            match targetModule.ContainingProjectModule with
            | :? IAssembly as assembly -> assembly.AssemblyName
            | :? IProject as project -> project.GetOutputAssemblyNameInfo(targetModule.TargetFrameworkId)
            | _ -> failwithf "mkIlScopeRef: %O -> %O" fromModule targetModule
        getAssemblyScope assemblyName

    let literalTypes =
        let unboxF f = unbox >> f
        [ PredefinedType.BOOLEAN_FQN, unboxF ILFieldInit.Bool
          PredefinedType.CHAR_FQN,    unboxF ILFieldInit.Char
          PredefinedType.SBYTE_FQN,   unboxF ILFieldInit.Int8
          PredefinedType.BYTE_FQN,    unboxF ILFieldInit.UInt8
          PredefinedType.SHORT_FQN,   unboxF ILFieldInit.Int16
          PredefinedType.USHORT_FQN,  unboxF ILFieldInit.UInt16
          PredefinedType.INT_FQN,     unboxF ILFieldInit.Int32
          PredefinedType.UINT_FQN,    unboxF ILFieldInit.UInt32
          PredefinedType.LONG_FQN,    unboxF ILFieldInit.Int64
          PredefinedType.ULONG_FQN,   unboxF ILFieldInit.UInt64
          PredefinedType.FLOAT_FQN,   unboxF ILFieldInit.Single
          PredefinedType.DOUBLE_FQN,  unboxF ILFieldInit.Double ] |> dict

    let nullLiteralValue = Some ILFieldInit.Null

    let mkLiteralValue (value: ConstantValue): ILFieldInit option =
        if value.IsBadValue() then None else
        if value.IsNull() then nullLiteralValue else

        // A separate case to prevent interning string literals.
        if value.IsString() then Some (ILFieldInit.String (unbox value.Value)) else

        match value.Type with
        | :? IDeclaredType as declaredType ->
            let mutable literalType = Unchecked.defaultof<_>
            match literalTypes.TryGetValue(declaredType.GetClrName(), &literalType) with
            | true -> literalValues.Intern(Some (literalType value.Value))
            | _ -> None
        | _ -> None

    let getAssemblyTypeRefCache (targetModule: IPsiModule) =
        let mutable cache = Unchecked.defaultof<_>
        match assemblyTypeRefs.TryGetValue(targetModule, &cache) with
        | true -> cache
        | _ ->

        let cache = ConcurrentDictionary()
        assemblyTypeRefs.[targetModule] <- cache
        cache

    member x.GetTypeRef(fromModule: IPsiModule, typeElement: ITypeElement) =
        let clrTypeName = typeElement.GetClrName()
        let targetModule = typeElement.Module

        let typeRefCache =
            if fromModule == targetModule then localTypeRefs else
            getAssemblyTypeRefCache targetModule

        let mutable typeRef = Unchecked.defaultof<_>
        match typeRefCache.TryGetValue(clrTypeName, &typeRef) with
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

                // The namespace is later splitted back by FCS during module import.
                // todo: rewrite this in FCS: add extension point, provide splitted namespaces
                let ns = clrTypeName.GetNamespaceName()
                if ns.IsEmpty() then enclosingTypeNames else

                match enclosingTypeNames with
                | hd :: tl -> String.Concat(ns, ".", hd) :: tl
                | [] -> failwithf "mkTypeRef: %O" clrTypeName

            let name =
                match containingType with
                | null -> clrTypeName.FullName
                | _ -> mkNameFromClrTypeName clrTypeName

            ILTypeRef.Create(scopeRef, enclosingTypes, name)

        typeRefCache.[clrTypeName.GetPersistent()] <- typeRef
        typeRef

    override x.Execute(args) =
        for change in args.ChangeMap.GetChanges<PsiModuleChange>() do
            for change in change.ModuleChanges do
                match change.Type with
                | PsiModuleChange.ChangeType.Removed ->
                    assemblyTypeRefs.TryRemove(change.Item) |> ignore
                | _ -> ()


type PreTypeDef(clrTypeName: IClrTypeName, moduleReader: ModuleReader) =
    member x.Name =
        let typeName = clrTypeName.TypeNames.Last()
        mkNameFromTypeNameAndParamsNumber typeName

    interface ILPreTypeDef with
        member x.Name = x.Name

        member x.Namespace =
            if clrTypeName.TypeNames.IsSingle() then [] else
            clrTypeName.NamespaceNames |> List.ofSeq

        member x.GetTypeDef() =
            match moduleReader.SymbolScope.GetTypeElementByCLRName(clrTypeName) with
            | null ->
                // The type doesn't exist in the module anymore.
                // It means the project has changed and FCS will be invalidating this module.
                mkDummyTypeDef x.Name

            // When there are multiple types with given clr name in the module we'll get some first one here.
            // todo: add a test for the case above
            | typeElement ->
                use cookie = CompilationContextCookie.GetOrCreate(moduleReader.Module.GetContextFromModule())
                mkDummyTypeDef typeElement.ShortName // todo


let mkDummyTypeDef name: ILTypeDef =
    let attributes = TypeAttributes.Public ||| TypeAttributes.Class
    let implements = []
    let genericParams = []
    let extends = None
    let nestedTypes = emptyILTypeDefs

    ILTypeDef
        (name, attributes, ILTypeDefLayout.Auto, implements, genericParams, extends, emptyILMethods, nestedTypes,
         emptyILFields, emptyILMethodImpls, emptyILEvents, emptyILProperties, emptyILSecurityDecls, emptyILCustomAttrs)


let typeParameterCountStrings = [| "`1"; "`2"; "`3"; "`4"; "`5"; "`6"; "`7" |]
let typeParameterCountStringsCount = typeParameterCountStrings.Length

let mkTypeName (name: string) (paramsCount: int): string =
    if paramsCount = 0 then name else
    let paramsCountString =
        if paramsCount >= typeParameterCountStringsCount then paramsCount.ToString() else
        typeParameterCountStrings.[paramsCount]
    name + paramsCountString

let mkNameFromTypeNameAndParamsNumber (nameAndParametersCount: TypeNameAndTypeParameterNumber) =
    mkTypeName nameAndParametersCount.TypeName nameAndParametersCount.TypeParametersNumber

let mkNameFromClrTypeName (clrTypeName: IClrTypeName) =
    mkTypeName clrTypeName.ShortName clrTypeName.TypeParametersCount


let isDll (project: IProject) =
    match project.ProjectProperties.BuildSettings with
    | :? IManagedProjectBuildSettings as buildSettings ->
        buildSettings.OutputType = ProjectOutputType.LIBRARY
    | _ -> false


module DummyModuleDefValues =
    let subsystemVersion = 4, 0
    let useHighEntropyVA = false
    let hashalg = None
    let locale = None
    let flags = 0
    let exportedTypes = mkILExportedTypes []
    let metadataVersion = String.Empty
