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

            | _ -> failwithf "mkType: resolved element: %O" typ

        | :? IArrayType as arrayType ->
            let elementType = mkType fromModule arrayType.ElementType
            let shape = ILArrayShape.FromRank(arrayType.Rank) // todo: add tests for different dimensions
            ILType.Array (shape, elementType)

        | :? IPointerType as pointerType ->
            let elementType = mkType fromModule pointerType.ElementType
            ILType.Ptr elementType // todo: intern

        | _ -> failwithf "mkType: type: %O" typ

    let mkEnumInstanceValue (enum: IEnum): ILFieldDef =
        let name = "value__"
        let fieldType =
            let enumType =
                let enumType = enum.GetUnderlyingType()
                if not enumType.IsUnknown then enumType else
                psiModule.GetPredefinedType().Int :> _
            mkType psiModule enumType
        let attributes = FieldAttributes.Public ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName
        ILFieldDef(name, fieldType, attributes, None, None, None, None, emptyILCustomAttrs)

    let mkParam (fromModule: IPsiModule) (param: IParameter): ILParameter =
        let name = param.ShortName
        let paramType = mkType fromModule param.Type

        let defaultValue =
            let defaultValue = param.GetDefaultValue()
            if defaultValue.IsBadValue then None else
            cache.GetLiteralValue(defaultValue.ConstantValue, defaultValue.DefaultTypeValue) // todo: add test

        { Name = Some name // todo: intern?
          Type = paramType
          Default = defaultValue
          Marshal = None // todo: used in infos.fs
          IsIn = param.Kind.Equals(ParameterKind.INPUT) // todo: add test
          IsOut = param.Kind.Equals(ParameterKind.OUTPUT) // todo: add test
          IsOptional = param.IsOptional
          CustomAttrsStored = storeILCustomAttrs emptyILCustomAttrs // todo
          MetadataIndex = NoMetadataIdx }

    let mkMethodAux name ret parameters methodAttrs genericParams (meth: IFunction): ILMethodDef =
        let implAttributes = MethodImplAttributes.Managed
        let callingConv = mkCallingConv meth
        let body = methodBodyUnavailable
        let securityDecls = emptyILSecurityDecls
        let isEntryPoint = false
        let customAttrs = emptyILCustomAttrs

        ILMethodDef
            (name, methodAttrs, implAttributes, callingConv, parameters, ret, body, isEntryPoint, genericParams,
             securityDecls, customAttrs)

    let mkCtor (fromModule: IPsiModule) (ctor: IConstructor): ILMethodDef =
        let name = DeclaredElementConstants.CONSTRUCTOR_NAME
        let attributes = mkMethodAttributes ctor ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName

        let parameters =
            ctor.Parameters
            |> List.ofSeq
            |> List.map (mkParam fromModule)

        mkMethodAux name voidReturn parameters attributes [] ctor

    let mkGenericVariance (variance: TypeParameterVariance): ILGenericVariance =
        match variance with
        | TypeParameterVariance.IN -> ILGenericVariance.ContraVariant
        | TypeParameterVariance.OUT -> ILGenericVariance.CoVariant
        | _ -> ILGenericVariance.NonVariant

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

    let mkField (fromModule: IPsiModule) (field: IField): ILFieldDef =
        let name = field.ShortName
        let attributes = mkFieldAttributes field

        let fieldType = mkType fromModule field.Type
        let data = None
        let offset = None

        let valueType =
            if not field.IsEnumMember then field.Type else
            match field.GetContainingType() with
            | :? IEnum as enum -> enum.GetUnderlyingType()
            | _ -> null

        let literalValue = cache.GetLiteralValue(field.ConstantValue, valueType)
        let marshal = None
        let customAttrs = mkILCustomAttrs []

        ILFieldDef(name, fieldType, attributes, data, literalValue, offset, marshal, customAttrs)

    let mkTypeDef (typeElement: ITypeElement) (clrTypeName: IClrTypeName) (moduleReader: ModuleReader): ILTypeDef = // todo: move reader out
        use compilationCookie = CompilationContextCookie.GetOrCreate(psiModule.GetContextFromModule())
        // todo: read members lazily

        let name =
            match typeElement.GetContainingType() with
            | null -> clrTypeName.FullName
            | _ -> mkNameFromClrTypeName clrTypeName

        let extends =
            // todo: intern
            match typeElement with
            | :? IClass as c ->
                match c.GetBaseClassType() with
                | null -> Some (mkType psiModule (psiModule.GetPredefinedType().Object))
                | baseType -> Some (mkType psiModule baseType)

            | :? IInterface -> None

            // todo: check structs, enums, delegates
            | _ ->
                let superTypes =
                    typeElement.GetSuperTypes()
                    |> Seq.map (fun t -> t.GetTypeElement())

                match superTypes.WhereNotNull().OfType<IClass>().FirstOrDefault() with
                | null -> Some (mkType psiModule (psiModule.GetPredefinedType().Object))
                | baseType -> Some (mkType psiModule (TypeFactory.CreateType(baseType)))

        let methods =
            typeElement.Constructors
            |> List.ofSeq
            |> List.map (mkCtor psiModule)
            |> mkILMethods

        let nestedTypes =
            let preTypeDefs =
                typeElement.NestedTypes
                |> Array.ofSeq
                |> Array.map (fun typeElement ->
                    PreTypeDef(typeElement.GetClrName().GetPersistent(), moduleReader) :> ILPreTypeDef)
            mkILTypeDefsComputed (fun _ -> preTypeDefs)

        let fields =
            let fields =
                match typeElement with
                | :? IClass as c -> c.Fields |> Seq.append c.Constants
                | :? IEnum as e -> e.EnumMembers :> _
                | :? IStruct as s -> s.Fields |> Seq.append s.Constants
                | _ -> EmptyList.Instance :> _

            match fields |> List.ofSeq with
            | [] -> emptyILFields
            | fields ->

            let fields = fields |> List.map (mkField psiModule)
            match typeElement with
            | :? IEnum as enum -> (mkEnumInstanceValue enum :: fields)
            | _ -> fields
            |> mkILFields

        let attributes = mkTypeAttributes typeElement

        let implements = []
        let genericParams =
            typeElement.TypeParameters
            |> List.ofSeq
            |> List.map (mkGenericParameterDef psiModule)

        ILTypeDef
            (name, attributes, ILTypeDefLayout.Auto, implements, genericParams, extends, methods, nestedTypes,
             fields, emptyILMethodImpls, emptyILEvents, emptyILProperties, emptyILSecurityDecls, emptyILCustomAttrs)

    member x.GetTypeDef(clrTypeName: IClrTypeName) =
        use cookie = ReadLockCookie.Create()
        match symbolScope.GetTypeElementByCLRName(clrTypeName) with
        | null ->
            // The type doesn't exist in the module anymore.
            // It should mean the project has changed and FCS will be invalidating this module.
            mkDummyTypeDef clrTypeName.ShortName

        // When there are multiple types with given clr name in the module we'll get some first one here.
        // todo: add a test for the case above
        | typeElement ->
            mkTypeDef typeElement clrTypeName x

    member x.Module = psiModule
    member x.SymbolScope = symbolScope

    // Initial value is an arbitrary timestamp that is earlier than any file modifications observed by FCS.
    member val TimeStamp =
        use cookie = ReadLockCookie.Create()
        psiModule.SourceFiles
        |> Seq.map (fun sf -> DateTime(sf.GetAggregatedTimestamp()))
        |> Seq.sort
        |> Seq.tryLast
        |> Option.defaultValue DateTime.MinValue

    interface ILModuleReader with
        member x.ILModuleDef =
            match cachedModuleDef with
            | Some moduleDef -> moduleDef
            | None ->

            use readLockCookie = ReadLockCookie.Create()

            // todo: change this when reading assemblies as well
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

    /// References to type in the same module.
    let localTypeRefs = ConcurrentDictionary<IClrTypeName, ILTypeRef>()

    /// References to types in a different assemblies (currently keyed by primary psi module).
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

    let getAssemblyTypeRefCache (targetModule: IPsiModule) =
        let mutable cache = Unchecked.defaultof<_>
        match assemblyTypeRefs.TryGetValue(targetModule, &cache) with
        | true -> cache
        | _ ->

        let cache = ConcurrentDictionary()
        assemblyTypeRefs.[targetModule] <- cache
        cache

    member x.GetLiteralValue(value: ConstantValue, valueType: IType): ILFieldInit option =
        if value.IsBadValue() then None else
        if value.IsNull() then nullLiteralValue else

        // A separate case to prevent interning string literals.
        if value.IsString() then Some (ILFieldInit.String (unbox value.Value)) else

        match valueType with
        | :? IDeclaredType as declaredType ->
            let mutable literalType = Unchecked.defaultof<_>
            match literalTypes.TryGetValue(declaredType.GetClrName(), &literalType) with
            | true -> literalValues.Intern(Some (literalType value.Value))
            | _ -> None
        | _ -> None

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


type PreTypeDef(clrTypeName: IClrTypeName, reader: ModuleReader) =
    member x.Name =
        let typeName = clrTypeName.TypeNames.Last()
        mkNameFromTypeNameAndParamsNumber typeName

    interface ILPreTypeDef with
        member x.Name = x.Name

        member x.Namespace =
            if not (clrTypeName.TypeNames.IsSingle()) then [] else
            clrTypeName.NamespaceNames |> List.ofSeq

        member x.GetTypeDef() =
            reader.GetTypeDef(clrTypeName)


let mkDummyTypeDef name: ILTypeDef =
    let failOnDummyTypeDefs = true // todo: move to a component, fail in tests only

    if failOnDummyTypeDefs then
        failwithf "mkDummyTypeDef %O" name

    let attributes = enum 0
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


let mkFieldAttributes (field: IField): FieldAttributes =
    let accessRights =
        match field.GetAccessRights() with
        | AccessRights.PUBLIC -> FieldAttributes.Public
        | AccessRights.INTERNAL -> FieldAttributes.Assembly
        | AccessRights.PRIVATE -> FieldAttributes.Private
        | AccessRights.PROTECTED -> FieldAttributes.Family
        | AccessRights.PROTECTED_OR_INTERNAL -> FieldAttributes.FamORAssem
        | AccessRights.PROTECTED_AND_INTERNAL -> FieldAttributes.FamANDAssem
        | _ -> enum 0

    accessRights |||
    (if field.IsStatic then FieldAttributes.Static else enum 0) |||
    (if field.IsReadonly then FieldAttributes.InitOnly else enum 0) |||
    (if field.IsConstant || field.IsEnumMember then FieldAttributes.Literal else enum 0)


let mkMethodAttributes (method: IFunction): MethodAttributes =
    let accessRights =
        match method.GetAccessRights() with
        | AccessRights.PUBLIC -> MethodAttributes.Public
        | AccessRights.INTERNAL -> MethodAttributes.Assembly
        | AccessRights.PRIVATE -> MethodAttributes.Private
        | AccessRights.PROTECTED -> MethodAttributes.Family
        | AccessRights.PROTECTED_OR_INTERNAL -> MethodAttributes.FamORAssem
        | AccessRights.PROTECTED_AND_INTERNAL -> MethodAttributes.FamANDAssem
        | _ -> enum 0

    accessRights |||
    MethodAttributes.HideBySig |||
    (if method.IsStatic then MethodAttributes.Static else enum 0) |||
    (if method.IsSealed then MethodAttributes.Final else enum 0) |||
    (if method.IsVirtual then MethodAttributes.Virtual else enum 0) |||
    (if not (method.GetHiddenMembers().IsEmpty()) then MethodAttributes.NewSlot else enum 0)


let mkTypeAccessRights (typeElement: ITypeElement): TypeAttributes =
    match typeElement with
    | :? IAccessRightsOwner as accessRightsOwner ->
        let accessRights = accessRightsOwner.GetAccessRights()
        if isNull (typeElement.GetContainingType()) then
            match accessRights with
            | AccessRights.PUBLIC -> TypeAttributes.Public
            | _ -> enum 0
        else
            match accessRights with
            | AccessRights.PUBLIC -> TypeAttributes.NestedPublic
            | AccessRights.INTERNAL -> TypeAttributes.NestedAssembly
            | AccessRights.PROTECTED -> TypeAttributes.NestedFamily
            | AccessRights.PROTECTED_OR_INTERNAL -> TypeAttributes.NestedFamORAssem
            | AccessRights.PROTECTED_AND_INTERNAL -> TypeAttributes.NestedFamANDAssem
            | AccessRights.PRIVATE -> TypeAttributes.NestedPrivate
            | _ -> TypeAttributes.NestedAssembly
    | _ -> enum 0


let mkTypeAttributes (typeElement: ITypeElement): TypeAttributes =
    // These attributes are ignored by FCS when reading types: BeforeFieldInit.

    let kind =
        match typeElement with
        | :? IClass as c ->
            (if c.IsAbstract then TypeAttributes.Abstract else enum 0) |||
            (if c.IsSealed then TypeAttributes.Sealed else enum 0)

        | :? IInterface -> TypeAttributes.Interface
        | :? IEnum -> TypeAttributes.Sealed

        // todo: structs
        // todo: delegates
        | _ -> enum 0

    // todo: add tests
    let accessRights = mkTypeAccessRights typeElement

    kind ||| accessRights


let staticCallingConv = Callconv (ILThisConvention.Static, ILArgConvention.Default)
let instanceCallingConv = Callconv (ILThisConvention.Instance, ILArgConvention.Default)

let mkCallingConv (func: IFunction): ILCallingConv =
    if func.IsStatic then staticCallingConv else instanceCallingConv


let voidReturn = mkILReturn ILType.Void
let methodBodyUnavailable = mkMethBodyAux MethodBody.NotAvailable

module DummyModuleDefValues =
    let subsystemVersion = 4, 0
    let useHighEntropyVA = false
    let hashalg = None
    let locale = None
    let flags = 0
    let exportedTypes = mkILExportedTypes []
    let metadataVersion = String.Empty
