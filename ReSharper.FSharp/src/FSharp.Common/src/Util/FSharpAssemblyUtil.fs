[<AutoOpen; Extension>]
module JetBrains.ReSharper.Plugins.FSharp.Util.FSharpAssemblyUtil

open System.Collections.Generic
open System.IO
open System.Text
open JetBrains.Diagnostics
open JetBrains.Metadata.Reader.API
open JetBrains.ProjectModel
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Modules
open JetBrains.ReSharper.Resources.Shell
open JetBrains.Util

[<CompiledName("InterfaceDataVersionAttrTypeName")>]
let interfaceDataVersionAttrTypeName = clrTypeName "Microsoft.FSharp.Core.FSharpInterfaceDataVersionAttribute"

let isFSharpAssemblyKey = Key("IsFSharpAssembly")

[<Extension; CompiledName("IsFSharpAssembly")>]
let isFSharpAssembly (psiModule: IPsiModule) =
    match psiModule.ContainingProjectModule with
    | :? IProject -> false
    | _ ->

    match psiModule.GetData(isFSharpAssemblyKey) with
    | null ->
        use cookie = ReadLockCookie.Create()
        let attrs = psiModule.GetPsiServices().Symbols.GetModuleAttributes(psiModule)
        let isFSharpAssembly = attrs.HasAttributeInstance(interfaceDataVersionAttrTypeName, false)

        psiModule.PutData(isFSharpAssemblyKey, if isFSharpAssembly then BooleanBoxes.True else BooleanBoxes.False)
        isFSharpAssembly

    | value -> value == BooleanBoxes.True

let [<Literal>] signatureInfoResourceName = "FSharpSignatureInfo."
let [<Literal>] signatureInfoResourceNameOld = "FSharpSignatureData."

let canHaveExternalSignatureInfo (psiAssembly: IPsiAssembly) =
    psiAssembly.AssemblyName.Name = "FSharp.Core"

let isSignatureDataResource (manifestResource: IMetadataManifestResource) (compilationUnitName: outref<string>) =
    let name = manifestResource.Name
    if startsWith signatureInfoResourceName name then
        compilationUnitName <- name.Substring(signatureInfoResourceName.Length)
        true
    elif startsWith signatureInfoResourceNameOld name then
        compilationUnitName <- name.Substring(signatureInfoResourceNameOld.Length)
        true
    else false


type FSharpSignatureDataResource =
    { CompilationUnitName: string
      Resource: IMetadataManifestResource }


type BinaryReaderEx =
    inherit BinaryReader

    new (stream) =
        { inherit BinaryReader(stream) }

    new (stream, encoding) =
        { inherit BinaryReader(stream, encoding) }

    member x.ReadPackedInt() =
        base.Read7BitEncodedInt()


let readStream (stream: Stream) =
    let reader = new BinaryReaderEx(stream, Encoding.UTF8)

    // F# compiler uses "ccu" term for cross-compilation unit.
    let ccuRefNames =
        let ccuRefsNumber = reader.ReadPackedInt()
        let res = Array.zeroCreate ccuRefsNumber

        for i = 0 to ccuRefsNumber - 1 do
            match int (reader.ReadByte()) with
            | 0 -> res.[i] <- reader.ReadString()
            | separator -> failwithf "Encoded ccuRef, expecting 0, got: %d" separator

        res

    let encodedTypeDeclsNumber = reader.ReadPackedInt()
    let hasAnonRecordsDecls =
        // The negative number indicates there are anonymous record declarations.
        encodedTypeDeclsNumber < 0

    let typeDeclsNumber =
        if not hasAnonRecordsDecls then encodedTypeDeclsNumber
        else -encodedTypeDeclsNumber - 1

    let typeParameterDeclsNumber = reader.ReadPackedInt()
    let valueDeclsNumber = reader.ReadPackedInt()
    let anonRecordDeclsNumber = if hasAnonRecordsDecls then reader.ReadPackedInt() else 0

    let _ =
        ccuRefNames,
        typeDeclsNumber,
        typeParameterDeclsNumber,
        valueDeclsNumber,
        anonRecordDeclsNumber

    ()

let getFSharpSignatureInfos (psiModule: IPsiModule) =
    match psiModule.As<IAssemblyPsiModule>() with
    | null -> null
    | assemblyPsiModule ->

    let psiAssembly = assemblyPsiModule.Assembly
    let path = psiAssembly.Location
    if isNull path then null else

    Assertion.Assert(isFSharpAssembly psiModule, "isFSharpAssembly psiModule")

    let metadataLoader = new MetadataLoader()
    let metadataAssembly = metadataLoader.TryLoadFrom(path, JetFunc<_>.False)
    if isNull metadataAssembly then null else

    let resources =
        let result = List()
        for manifestResource in metadataAssembly.GetManifestResources() do
            let mutable compilationUnitName = Unchecked.defaultof<_>
            if isSignatureDataResource manifestResource &compilationUnitName then
                result.Add({ CompilationUnitName = compilationUnitName; Resource = manifestResource })
        result

    let resources =
        if not (resources.IsEmpty()) then resources else

        if canHaveExternalSignatureInfo psiAssembly then
            // todo: read an external signature data file
            resources
        else
            resources

    for signatureDataResource in resources do
        let manifestResource = signatureDataResource.Resource
        let mutable compilationUnitName = Unchecked.defaultof<_>
        if not (isSignatureDataResource manifestResource &compilationUnitName) then () else

        use stream = manifestResource.GetDisposition().CreateResourceReader()
        readStream stream

    null