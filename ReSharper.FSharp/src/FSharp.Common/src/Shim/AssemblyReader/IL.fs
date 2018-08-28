module JetBrains.ReSharper.Plugins.FSharp.Common.Shim.AssemblyReader.IL

open System
open JetBrains.Metadata.Reader.API
open JetBrains.Metadata.Utils
open System.Collections.Concurrent
open JetBrains.ProjectModel
open JetBrains.ProjectModel.Model2.Assemblies.Interfaces
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Modules
open JetBrains.Util.DataStructures
open Microsoft.FSharp.Compiler.AbstractIL.IL

let literalTypes =
    let unboxF f = unbox >> f
    [ PredefinedType.BOOLEAN_FQN, unboxF ILFieldInit.Bool
      PredefinedType.STRING_FQN,  unboxF ILFieldInit.String
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

let mkLiteralValue (value: ConstantValue): ILFieldInit option =
    if value.IsBadValue() then None else
    if value.IsNull() then Some ILFieldInit.Null else

    match value.Type with
    | :? IDeclaredType as declaredType ->
        match literalTypes.TryGetValue(declaredType.GetClrName()) with
        | true, valueCtor -> Some (valueCtor value.Value)
        | _ -> None
    | _ -> None

let mkGenericVariance (variance: TypeParameterVariance): ILGenericVariance =
    match variance with
    | TypeParameterVariance.INVARIANT -> ILGenericVariance.NonVariant
    | TypeParameterVariance.IN -> ILGenericVariance.ContraVariant
    | TypeParameterVariance.OUT -> ILGenericVariance.CoVariant
    | _ -> failwithf "mkGenericVariance %O" variance

let mkGenericParameterDef (m: IPsiModule) (typeParameter: ITypeParameter): ILGenericParameterDef =
    let typeConstraints = []
    let attributes = storeILCustomAttrs emptyILCustomAttrs

    { Name = typeParameter.ShortName
      Constraints = typeConstraints
      Variance = mkGenericVariance typeParameter.Variance
      HasReferenceTypeConstraint = typeParameter.IsClassType
      HasNotNullableValueTypeConstraint = typeParameter.IsValueType
      HasDefaultConstructorConstraint = typeParameter.HasDefaultConstructor
      CustomAttrsStored = attributes
      MetadataIndex = NoMetadataIdx }


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
