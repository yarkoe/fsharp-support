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
open JetBrains.ReSharper.Psi.Util
open JetBrains.ReSharper.Resources.Shell
open JetBrains.Threading
open JetBrains.Util
open JetBrains.Util.DataStructures
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader

type ModuleReader(psiModule: IPsiModule, cache: ModuleReaderCache) =
    let symbolScope = psiModule.GetPsiServices().Symbols.GetSymbolScope(psiModule, false, true)

    let locker = JetFastSemiReenterableRWLock()

    let mutable moduleDef: ILModuleDef option = None

    // Initial value is an arbitrary timestamp that is earlier than any file modifications observed by FCS.
    let mutable timeStamp = DateTime.MinValue

    /// Type definitions imported by FCS.
    let typeDefs = ConcurrentDictionary<IClrTypeName, ILTypeDef>()

    // todo: intern types
    let rec mkType (typ: IType): ILType =
        if typ.IsVoid() then ILType.Void else

        match typ with
        | :? IDeclaredType as dt ->
            match dt.Resolve() with
            | :? EmptyResolveResult ->
                // todo: add per-module singletons for predefines types
                mkType (psiModule.GetPredefinedType().Object)

            | resolveResult ->

            match resolveResult.DeclaredElement with
            | :? ITypeParameter as typeParameter ->
                match typeParameter.Owner with
                | null -> mkType (psiModule.GetPredefinedType().Object)
                | owner ->

                let mutable index = typeParameter.Index
                let mutable parent = typeParameter.Owner.GetContainingType()
                while isNotNull parent do
                    index <- index + parent.TypeParameters.Count
                    parent <- parent.GetContainingType()

                ILType.TypeVar (uint16 index)

            | :? ITypeElement as typeElement ->
                let typeArgs =
                    resolveResult.Substitution.Domain
                    |> Seq.map (fun typeParameter -> mkType resolveResult.Substitution.[typeParameter])
                    |> List.ofSeq

                let typeRef = cache.GetTypeRef(psiModule, typeElement)
                let typeSpec = ILTypeSpec.Create(typeRef, typeArgs)

                match typeElement with
                | :? IEnum
                | :? IStruct ->
                    ILType.Value typeSpec

                | _ -> ILType.Boxed typeSpec

            | _ -> failwithf "mkType: resolved element: %O" typ

        | :? IArrayType as arrayType ->
            let elementType = mkType arrayType.ElementType
            let shape = ILArrayShape.FromRank(arrayType.Rank)
            ILType.Array (shape, elementType)

        | :? IPointerType as pointerType ->
            let elementType = mkType pointerType.ElementType
            ILType.Ptr elementType

        | _ -> failwithf "mkType: type: %O" typ


    let mkEnumInstanceValue (enum: IEnum): ILFieldDef =
        let name = "value__"
        let fieldType =
            let enumType =
                let enumType = enum.GetUnderlyingType()
                if not enumType.IsUnknown then enumType else
                psiModule.GetPredefinedType().Int :> _
            mkType enumType
        let attributes = FieldAttributes.Public ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName
        ILFieldDef(name, fieldType, attributes, None, None, None, None, emptyILCustomAttrs)


    let mkMethodRef (method: IFunction): ILMethodRef =
        let typeRef =
            let typeElement =
                match method.GetContainingType() with
                | null -> psiModule.GetPredefinedType().Object.GetTypeElement()
                | typeElement -> typeElement

            cache.GetTypeRef(psiModule, typeElement)
        
        let callingConv = mkCallingConv method
        let name = method.ShortName

        let typeParamsCount =
            match method with
            | :? IMethod as method -> method.TypeParameters.Count
            | _ -> 0

        let paramTypes =
            method.Parameters
            |> List.ofSeq
            |> List.map (fun param -> mkType param.Type)
        
        let returnType = mkType method.ReturnType
        
        ILMethodRef.Create(typeRef, callingConv, name, typeParamsCount, paramTypes, returnType)


    let blobProlog = [| 1uy; 0uy |]
    let emptyAttributeBlob = [| 1uy; 0uy; 0uy; 0uy |]

    let mkDefaultCtorAttribute (attrType: IClrTypeName): ILAttribute =
        use compilationCookie = CompilationContextCookie.GetOrCreate(psiModule.GetContextFromModule())
        let attrType = TypeFactory.CreateTypeByCLRName(attrType, psiModule)
        match attrType.GetTypeElement() with
        | null -> failwithf "getting param array type element in %O" psiModule // todo: safer handling
        | typeElement ->

        let attrType = mkType attrType
        let ctor = typeElement.Constructors.First(fun ctor -> ctor.IsParameterless) // todo: safer handling
        let ctorMethodRef = mkMethodRef ctor

        let methodSpec = ILMethodSpec.Create(attrType, ctorMethodRef, [])
        { Method = methodSpec
          Data = emptyAttributeBlob
          Elements = [] }

    let paramArrayAttribute = mkDefaultCtorAttribute PredefinedType.PARAM_ARRAY_ATTRIBUTE_CLASS
    let extensionAttribute = mkDefaultCtorAttribute PredefinedType.EXTENSION_ATTRIBUTE_CLASS


    let mkAttribute (attrInstance: IAttributeInstance): ILAttribute =
        let attrCtor = attrInstance.Constructor
        let attrType = attrInstance.GetAttributeType() |> mkType

        let ctorMethodRef = mkMethodRef attrCtor
        let ctorMethodSpec = ILMethodSpec.Create(attrType, ctorMethodRef, [])

        let data = List()
        data.AddRange(blobProlog)

        data.Add(byte attrInstance.NamedParameterCount)

        { Method = ctorMethodSpec
          Data = data.ToArray()
          Elements = [] }


    let mkParam (fromModule: IPsiModule) (param: IParameter): ILParameter =
        let name = param.ShortName
        let paramType = mkType param.Type

        let defaultValue =
            let defaultValue = param.GetDefaultValue()
            if defaultValue.IsBadValue then None else
            cache.GetLiteralValue(defaultValue.ConstantValue, defaultValue.DefaultTypeValue) // todo: add test

        let attrs =
            let attrs: ILAttribute list = []
            if param.IsParameterArray then paramArrayAttribute :: attrs else
            attrs

        { Name = Some name // todo: intern?
          Type = paramType
          Default = defaultValue
          Marshal = None // todo: used in infos.fs
          IsIn = param.Kind.Equals(ParameterKind.INPUT) // todo: add test
          IsOut = param.Kind.Equals(ParameterKind.OUTPUT) // todo: add test
          IsOptional = param.IsOptional
          CustomAttrsStored = attrs |> mkILCustomAttrs |> storeILCustomAttrs
          MetadataIndex = NoMetadataIdx }

    let mkParams (method: IFunction): ILParameter list =
        method.Parameters
        |> List.ofSeq
        |> List.map (mkParam psiModule)


    let mkGenericVariance (variance: TypeParameterVariance): ILGenericVariance =
        match variance with
        | TypeParameterVariance.IN -> ILGenericVariance.ContraVariant
        | TypeParameterVariance.OUT -> ILGenericVariance.CoVariant
        | _ -> ILGenericVariance.NonVariant

    let mkGenericParameterDef (fromModule: IPsiModule) (typeParameter: ITypeParameter): ILGenericParameterDef =
        let typeConstraints =
            typeParameter.TypeConstraints
            |> List.ofSeq
            |> List.map mkType

        let attributes = storeILCustomAttrs emptyILCustomAttrs // todo

        { Name = typeParameter.ShortName
          Constraints = typeConstraints
          Variance = mkGenericVariance typeParameter.Variance
          HasReferenceTypeConstraint = typeParameter.IsClassType
          HasNotNullableValueTypeConstraint = typeParameter.IsValueType
          HasDefaultConstructorConstraint = typeParameter.HasDefaultConstructor
          CustomAttrsStored = attributes
          MetadataIndex = NoMetadataIdx }


    let mkMethod (fromModule: IPsiModule) (method: IFunction): ILMethodDef =
        let name = method.ShortName
        let methodAtts = mkMethodAttributes method
        let callingConv = mkCallingConv method
        let parameters = mkParams method

        let ret =
            let returnType = method.ReturnType
            if returnType.IsVoid() then voidReturn else

            mkType returnType |> mkILReturn

        let genericParams =
            match method with
            | :? IMethod as method ->
                method.TypeParameters
                |> Seq.map (mkGenericParameterDef fromModule)
                |> List.ofSeq
            | _ -> []

        let attrs =
            match method with
            | :? IMethod as method when method.IsExtensionMethod -> [extensionAttribute]
            | _ -> []
            |> mkILCustomAttrs

        let implAttributes = MethodImplAttributes.Managed
        let body = methodBodyUnavailable
        let securityDecls = emptyILSecurityDecls
        let isEntryPoint = false

        ILMethodDef
            (name, methodAtts, implAttributes, callingConv, parameters, ret, body, isEntryPoint, genericParams,
             securityDecls, attrs)


    let mkField (fromModule: IPsiModule) (field: IField): ILFieldDef =
        let name = field.ShortName
        let attributes = mkFieldAttributes field

        let fieldType = mkType field.Type
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

    let mkProperty (property: IProperty): ILPropertyDef =
        let name = property.ShortName
        let attrs = enum 0 // todo
        let callConv = mkCallingThisConv property
        let propertyType = mkType property.Type
        let init = None // todo
        let args =
            property.Parameters
            |> List.ofSeq
            |> List.map (fun p -> mkType p.Type)

        let setter =
            match property.Setter with
            | null -> None
            | setter -> Some (mkMethodRef setter)

        let getter =
            match property.Getter with
            | null -> None
            | getter -> Some (mkMethodRef getter)

        ILPropertyDef(name, attrs, setter, getter, callConv, propertyType, init, args, emptyILCustomAttrs)


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
                | null -> Some (mkType (psiModule.GetPredefinedType().Object))
                | baseType -> Some (mkType baseType)

            | :? IInterface -> None

            // todo: check structs, enums, delegates
            | _ ->
                let superTypes =
                    typeElement.GetSuperTypes()
                    |> Seq.map (fun t -> t.GetTypeElement())

                match superTypes.WhereNotNull().OfType<IClass>().FirstOrDefault() with
                | null -> Some (mkType (psiModule.GetPredefinedType().Object))
                | baseType -> Some (mkType (TypeFactory.CreateType(baseType)))

        let methods =
            typeElement.GetMembers().OfType<IFunction>()
            |> List.ofSeq
            |> List.map (mkMethod psiModule)
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

        let properties =
            typeElement.Properties
            |> List.ofSeq
            |> List.map (mkProperty)
            |> mkILProperties

        let typeAttributes = mkTypeAttributes typeElement

        let implements =
            typeElement.GetSuperTypesWithoutCircularDependent()
            |> List.ofSeq
            |> List.filter (fun t -> let typeElement = t.GetTypeElement() in typeElement :? IInterface)
            |> List.map mkType

        let genericParams =
            typeElement.GetAllTypeParameters()
            |> List.ofSeq
            |> List.rev
            |> List.map (mkGenericParameterDef psiModule)

        let attributes =
            if not (typeElement.Methods |> Seq.exists (fun m -> m.IsExtensionMethod)) then [] else [extensionAttribute]
            |> mkILCustomAttrs

        ILTypeDef
            (name, typeAttributes, ILTypeDefLayout.Auto, implements, genericParams, extends, methods, nestedTypes,
             fields, emptyILMethodImpls, emptyILEvents, properties, emptyILSecurityDecls, attributes)

    member x.TryGetCachedTypeDef(clrTypeName: IClrTypeName, typeDef: byref<ILTypeDef>) =
        use lock = locker.UsingReadLock()
        typeDefs.TryGetValue(clrTypeName, &typeDef)

    member x.GetTypeDef(clrTypeName: IClrTypeName) =
        let mutable typeDef = Unchecked.defaultof<_>
        match x.TryGetCachedTypeDef(clrTypeName, &typeDef) with
        | true -> typeDef
        | _ ->

        use lock = locker.UsingWriteLock()
        use cookie = ReadLockCookie.Create()
        match symbolScope.GetTypeElementByCLRName(clrTypeName) with
        | null ->
            // The type doesn't exist in the module anymore.
            // It should mean the project has changed and FCS will be invalidating this module.
            let dummyTypeDefProvider = psiModule.GetSolution().GetComponent<IDummyTypeDefProvider>()
            dummyTypeDefProvider.GetTypeDef(clrTypeName.ShortName)

        // When there are multiple types with given clr name in the module we'll get some first one here.
        // todo: add a test for the case above
        | typeElement ->
            let typeDef = mkTypeDef typeElement clrTypeName x
            typeDefs.[clrTypeName] <- typeDef
            typeDef

    member x.InvalidateTypeDef(clrTypeName: IClrTypeName) =
        use lock = locker.UsingWriteLock()
        typeDefs.TryRemove(clrTypeName) |> ignore
        moduleDef <- None
        timeStamp <- DateTime.UtcNow

    member x.Module =
        use lock = locker.UsingReadLock()
        psiModule

    member x.TimeStamp =
        use lock = locker.UsingReadLock()
        timeStamp

    interface ILModuleReader with
        member x.ILModuleDef =
            match moduleDef with
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

            // todo: add internals visible to test
            let flags = 0 // todo
            let exportedTypes = mkILExportedTypes []

            let newModuleDef =
                mkILSimpleModule
                    assemblyName moduleName isDll
                    DummyModuleDefValues.subsystemVersion
                    DummyModuleDefValues.useHighEntropyVA
                    typeDefs
                    None None flags exportedTypes
                    DummyModuleDefValues.metadataVersion

            moduleDef <- Some newModuleDef
            newModuleDef

        member x.ILAssemblyRefs = []
        member x.Dispose() = ()


/// A common cache shared by all module readers.
[<SolutionComponent>]
type ModuleReaderCache(lifetime, changeManager) =
    inherit ChangeListenerBase(lifetime, changeManager)

    let assemblyRefs = ConcurrentDictionary<AssemblyNameInfo, ILScopeRef>()

    /// References to types in the same module.
    let localTypeRefs = ConcurrentDictionary<IClrTypeName, ILTypeRef>()

    /// References to types in a different assemblies (currently keyed by primary psi module).
    let assemblyTypeRefs = ConcurrentDictionary<IPsiModule, ConcurrentDictionary<IClrTypeName, ILTypeRef>>()

    let cultures = DataIntern()
    let publicKeys = DataIntern()
    let literalValues = DataIntern()

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
            | null | "neutral" -> None
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


let typeParameterCountStrings = [| "`0"; "`1"; "`2"; "`3"; "`4"; "`5"; "`6"; "`7" |]
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


let methodSpecialNameAttrs =
    MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName

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
    (if not (method.GetHiddenMembers().IsEmpty()) then MethodAttributes.NewSlot else enum 0) |||
    (if method :? IConstructor || method :? IAccessor then methodSpecialNameAttrs else enum 0)


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

    // todo: ansi, sequential

    let kind =
        match typeElement with
        | :? IClass as c ->
            (if c.IsAbstract then TypeAttributes.Abstract else enum 0) |||
            (if c.IsSealed then TypeAttributes.Sealed else enum 0)

        | :? IInterface -> TypeAttributes.Interface

        | :? IEnum
        | :? IStruct
        | :? IDelegate -> TypeAttributes.Sealed

        | _ -> enum 0

    // todo: add tests
    let accessRights = mkTypeAccessRights typeElement

    kind ||| accessRights


let staticCallingConv = Callconv (ILThisConvention.Static, ILArgConvention.Default)
let instanceCallingConv = Callconv (ILThisConvention.Instance, ILArgConvention.Default)

let mkCallingConv (func: IFunction): ILCallingConv =
    if func.IsStatic then staticCallingConv else instanceCallingConv

let mkCallingThisConv (func: IModifiersOwner): ILThisConvention =
    if func.IsStatic then ILThisConvention.Static else ILThisConvention.Instance


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


type IDummyTypeDefProvider =
    abstract GetTypeDef: name: string -> ILTypeDef


[<SolutionComponent>]
type DummyTypeDefProvider() =
    interface IDummyTypeDefProvider with
        member x.GetTypeDef(name) =
            let attributes = enum 0
            let layout = ILTypeDefLayout.Auto
            let implements = []
            let genericParams = []
            let extends = None
            let nestedTypes = emptyILTypeDefs

            ILTypeDef
                (name, attributes, layout, implements, genericParams, extends, emptyILMethods, nestedTypes,
                 emptyILFields, emptyILMethodImpls, emptyILEvents, emptyILProperties, emptyILSecurityDecls,
                 emptyILCustomAttrs)
