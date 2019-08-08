[<AutoOpen; Extension>]
module JetBrains.ReSharper.Plugins.FSharp.Util.FSharpAssemblyUtil

open System.Collections.Generic
open System.IO
open System.Text
open JetBrains.Diagnostics
open JetBrains.Metadata.Reader.API
open JetBrains.ProjectModel
open JetBrains.ReSharper.Plugins.FSharp.Util
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


type FSharpMetadataReader(stream, encoding) =
    inherit BinaryReader(stream, encoding)

    let findUniqueString (reader: FSharpMetadataReader) =
        let id = reader.ReadPackedInt()
        reader.Strings.[id]

    override x.ReadString() =
        let len = x.ReadPackedInt()
        let bytes = x.ReadBytes(len)
        Encoding.UTF8.GetString(bytes)

    member x.ReadPackedInt() =
        let b0 = x.ReadByteAsInt()

        if b0 <= 0x7F then
            b0
        elif b0 <= 0xBF then
            let b0 = b0 &&& 0x7F
            let b1 = x.ReadByteAsInt()
            (b0 <<< 8) ||| b1
        else
            assert(b0 = 0xFF)

            let b0 = x.ReadByteAsInt()
            let b1 = x.ReadByteAsInt()
            let b2 = x.ReadByteAsInt()
            let b3 = x.ReadByteAsInt()
            b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)

    member x.IgnorePackedInt() =
        x.ReadPackedInt() |> ignore

    member x.ReadByteAsInt() =
        int (x.ReadByte())

    member x.ReadBool() =
        let b = x.ReadByteAsInt()
        b = 1

    member x.IgnoreByte() =
        x.ReadByte() |> ignore

    member x.IgnoreBool() =
        x.ReadBool() |> ignore

    member val Strings: string[] = null with get, set

    member val FindUniqueString = findUniqueString


let ignoreSpace n (reader: FSharpMetadataReader) =
    for i = 0 to n - 1 do
        reader.ReadByteAsInt() |> ignore

let ignoreUsedSpace f (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 1 ->
        f reader |> ignore
        ignoreSpace 1 reader
    | _ -> ()

let readArray f (reader: FSharpMetadataReader) =
    let len = reader.ReadPackedInt()
    let res = Array.zeroCreate len
    for i = 0 to len - 1 do
        res.[i] <- f reader
    res

let inline ignoreTuple2 f1 f2 (reader: FSharpMetadataReader) =
    f1 reader |> ignore
    f2 reader |> ignore

let inline ignoreTuple3 f1 f2 f3 (reader: FSharpMetadataReader) =
    f1 reader |> ignore
    f2 reader |> ignore
    f3 reader |> ignore

let inline ignoreList f (reader: FSharpMetadataReader) =
    let len = reader.ReadPackedInt()
    for i = 0 to len - 1 do
        f reader |> ignore

let ignoreNBytes n (reader: FSharpMetadataReader) =
    for i = 0 to n - 1 do
        reader.ReadByte() |> ignore

let readString (reader: FSharpMetadataReader) =
    reader.ReadString()

let readInt (reader: FSharpMetadataReader) =
    reader.ReadPackedInt()

let ignoreBool (reader: FSharpMetadataReader) =
    reader.IgnoreBool()

let ignoreBytes (reader: FSharpMetadataReader) =
    let n = readInt reader
    ignoreNBytes n reader

let ignoreIntAndIgnoreList f (reader: FSharpMetadataReader) =
    reader.IgnorePackedInt()
    ignoreList f reader

let readOption f (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> None
    | 1 -> Some (f reader)
    | n -> failwithf "readOption: %d" n

let ignoreOption f (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> ()
    | 1 -> f reader |> ignore
    | n -> failwithf "ignoreOption: %d" n

let ignoreRange (reader: FSharpMetadataReader) =
    reader.FindUniqueString reader |> ignore // path string id
    reader.IgnorePackedInt() // start line
    reader.IgnorePackedInt() // start column
    reader.IgnorePackedInt() // end line
    reader.IgnorePackedInt() // end column

let ignoreRanges (reader: FSharpMetadataReader) =
    ignoreRange reader
    ignoreRange reader

let ignoreIdent (reader: FSharpMetadataReader) =
    reader.FindUniqueString reader |> ignore // string id
    ignoreRange reader

let readIdentAsString (reader: FSharpMetadataReader) =
    let id = reader.FindUniqueString reader
    ignoreRange reader
    id

let ignoreMemberFlags (reader: FSharpMetadataReader) =
    reader.IgnoreByte() // is instance
    reader.IgnoreByte() // reserved space
    reader.IgnoreByte() // is dispatch slot
    reader.IgnoreByte() // is override or explicit impl
    reader.IgnoreByte() // is final
    reader.IgnoreByte() // member kind

let ignoreModuleRef (reader: FSharpMetadataReader) =
    reader.FindUniqueString reader |> ignore
    reader.IgnoreBool()
    ignoreOption ignoreBytes reader

let ignoreILPublicKey (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 | 1 -> ignoreBytes reader
    | n -> failwithf "ilPublicKey: %d" n

let ignoreILVersion (reader: FSharpMetadataReader) =
    reader.IgnorePackedInt()
    reader.IgnorePackedInt()
    reader.IgnorePackedInt()
    reader.IgnorePackedInt()

let ignoreAssemblyRef (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        reader.FindUniqueString reader |> ignore
        ignoreOption ignoreBytes reader
        ignoreILPublicKey reader
        reader.IgnoreBool()
        ignoreOption ignoreILVersion reader
        ignoreOption reader.FindUniqueString reader
    | n -> failwithf "ilAssemblyRef: %d" n

let ignoreILScopeRef (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> () // ILScopeRef.Local
    | 1 -> ignoreModuleRef reader // ILScopeRef.Module
    | 2 -> ignoreAssemblyRef reader // ILScopeRef.Assembly
    | n -> failwithf "ilScopeRef: %d" n

let ignoreILTypeRef (reader: FSharpMetadataReader) =
     ignoreILScopeRef reader
     ignoreList reader.FindUniqueString reader
     reader.FindUniqueString reader |> ignore

let ignoreIsType (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 | 1 | 2 -> ()
    | n -> failwithf "isType: %d" n

let ignoreCompilationPathPart (reader: FSharpMetadataReader) =
    reader.FindUniqueString reader |> ignore // name
    ignoreIsType reader

let ignoreCompilationPath (reader: FSharpMetadataReader) =
    ignoreILScopeRef reader
    ignoreList ignoreCompilationPathPart reader

let ignoreAccess (reader: FSharpMetadataReader) =
    ignoreList ignoreCompilationPath reader

let ignoreILBasicCallConv (reader: FSharpMetadataReader) =
    let n = reader.ReadByteAsInt()
    if n >= 0 && n <= 5 then () else
    failwithf "ilBasicCallConv: %d" n

let ignoreILHasThis (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 | 1 | 2 -> ()
    | n -> failwithf "ilHasThis: %d" n

let ignoreCallConv (reader: FSharpMetadataReader) =
    ignoreILHasThis reader
    ignoreILBasicCallConv reader

let ignoreTypeRef (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 | 1 -> reader.IgnorePackedInt()
    | n -> failwithf "typeRef: %d" n

let ignoreTypeKind (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 | 1 -> ()
    | n -> failwithf "typeKind: %d" n

let ignoreAnonRecordInfo (reader: FSharpMetadataReader) =
    reader.IgnorePackedInt() // info unique id
    reader.IgnorePackedInt() // ccu ref unique id
    reader.IgnoreBool() // is struct
    ignoreList ignoreIdent reader // field names

let rec ignoreMeasureExpr (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> ignoreTypeRef reader // Measure.Con
    | 1 -> ignoreMeasureExpr reader // Measure.Inv

    | 2 ->
        // Measure.Prod
        ignoreMeasureExpr reader
        ignoreMeasureExpr reader

    | 3 -> reader.IgnorePackedInt() // Measure.Var
    | 4 -> () // Measure.One

    | 5 ->
        // Measure.RationalPower
        reader.IgnorePackedInt()
        reader.IgnorePackedInt()

    | n -> failwithf "u_measure_expr: %d" n

let readRecordFieldRef (reader: FSharpMetadataReader) =
    ignoreTypeRef reader
    reader.FindUniqueString reader |> ignore // field ident

let ignoreConst (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> reader.IgnoreBool()  // bool
    | 1 -> reader.IgnorePackedInt() // int8
    | 2 -> reader.ReadByte() |> ignore // uint8 
    | 3 -> reader.IgnorePackedInt() // int16
    | 4 -> reader.IgnorePackedInt() // uint16
    | 5 -> reader.IgnorePackedInt() // int32
    | 6 -> reader.IgnorePackedInt() // uint32
    | 7 -> reader.ReadInt64() |> ignore // int64
    | 8 -> reader.ReadInt64() |> ignore // uint64
    | 9 -> reader.ReadInt64() |> ignore // intPtr
    | 10 -> reader.ReadInt64() |> ignore // uintPtr
    | 11 -> reader.IgnorePackedInt() // single
    | 12 -> reader.ReadInt64() |> ignore // double
    | 13 -> reader.ReadInt16() |> ignore // char
    | 14 -> reader.FindUniqueString reader |> ignore // string
    | 15 -> () // unit
    | 16 -> () // zero
    | 17 -> ignoreList readInt reader
    | _ -> failwith "u_const"

let rec ignoreType (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> ignoreList ignoreType reader // TType_tuple
    | 1 -> reader.IgnorePackedInt() // simple type from table

    | 2 ->
        // TType_app
        ignoreTypeRef reader
        ignoreList ignoreType reader

    | 3 ->
        // TType_fun
        ignoreType reader
        ignoreType reader

    | 4 -> reader.IgnorePackedInt() // type parameter from table

    | 5 ->
        // TType_forall
        ignoreList ignoreTypar reader
        ignoreType reader

    | 6 ->
        // TType_measure
        ignoreMeasureExpr reader

    | 7 ->
        // TType_ucase
        ignoreUnionCaseRef reader
        ignoreList ignoreType reader

    | 8 -> ignoreList ignoreType reader // TType_tuple

    | 9 ->
        //TType_anon
        ignoreAnonRecordInfo reader
        ignoreList ignoreType reader

    | n -> failwithf "type: %d" n


and ignoreExpr (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        //Expr.Const
        ignoreConst reader
        ignoreType reader

    | 1 ->
        //Expr.Val
        ignoreValRef reader
        reader.ReadByte() |> ignore // valUseFlag

    | 2 ->
        //Expr.Op
        ignoreOp reader
        ignoreList ignoreType reader
        ignoreList ignoreExpr reader

    | 3 ->
        //Expr.Sequential
        ignoreExpr reader
        ignoreExpr reader
        reader.IgnorePackedInt()

    | 4 ->
        //Expr.Lambda
        ignoreOption ignoreVal reader
        ignoreOption ignoreVal reader
        ignoreList ignoreVal reader
        ignoreExpr reader
        ignoreType reader

    | 5 ->
        //Expr.TyLambda
        ignoreList ignoreTypar reader
        ignoreExpr reader

    | 6 ->
        //Expr.App
        ignoreExpr reader
        ignoreType reader
        ignoreList ignoreType reader
        ignoreList ignoreExpr reader

    | 7 ->
        //Expr.LetRec
        ignoreList ignoreBind reader
        ignoreExpr reader

    | 8 ->
        //Expr.Let
        ignoreBind reader
        ignoreExpr reader

    | 9 ->
        //Expr.Match
        ignoreDecisionTree reader
        readTargets reader
        ignoreType reader

    | 10 ->
        //Expr.Obj
        ignoreType reader
        ignoreOption ignoreVal reader
        ignoreExpr reader
        ignoreList readMethod reader
        ignoreList readInterfaceImpl reader

    | 11 ->
        //Expr.StaticOptimization
        ignoreList readStaticOptimizationConstraint reader
        ignoreExpr reader
        ignoreExpr reader

    | 12 ->
        //Expr.TyChoose
        ignoreList ignoreTypar reader
        ignoreExpr reader

    | 13 ->
        //Expr.Quote
        ignoreExpr reader
        ignoreType reader

    | _ -> failwith "readExpr"

and ignoreOp (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        // TOp.UnionCase
        ignoreUnionCaseRef reader
    | 1 ->
        // TOp.ExnConstr
        ignoreTypeRef reader
    | 2 ->
        // TOp.Tuple
        ()
    | 3 ->
        // TOp.Recd
        ignoreTypeRef reader
    | 4 ->
        // TOp.ValFieldSet
        readRecordFieldRef reader
    | 5 ->
        // TOp.ValFieldGet
        readRecordFieldRef reader
    | 6 ->
        // TOp.UnionCaseTagGet
        ignoreTypeRef reader
    | 7 ->
        // TOp.UnionCaseFieldGet
        ignoreUnionCaseRef reader
        reader.IgnorePackedInt()
    | 8 ->
        // TOp.UnionCaseFieldSet
        ignoreUnionCaseRef reader
        reader.IgnorePackedInt()
    | 9 ->
        // TOp.ExnFieldGet
        ignoreTypeRef reader
        reader.IgnorePackedInt()
    | 10 ->
        // TOp.ExnFieldSet
        ignoreTypeRef reader
        reader.IgnorePackedInt()
    | 11 ->
        // TOp.TupleFieldGet
        reader.IgnorePackedInt()
    | 12 ->
        // todo :(
        // TOp.ILAsm
//        let a = (u_list u_ILInstr) st
//        let b = u_tys st
        ()
    | 13 ->
        // TOp.RefAddrGet
        ()
    | 14 ->
        // TOp.UnionCaseProof
        ignoreUnionCaseRef reader
    | 15 ->
        // TOp.Coerce
        ()
    | 16 ->
        // TOp.TraitCall
        ignoreTraitConstraint reader
    | 17 ->
        // TOp.LValueOp
        reader.IgnoreByte() // LValueOperation
        ignoreValRef reader
    | 18 ->
        // TOp.ILCall
        reader.IgnoreBool()
        reader.IgnoreBool()
        reader.IgnoreBool()
        reader.IgnoreBool()
        readValRefFlags reader
        reader.IgnoreBool()
        reader.IgnoreBool()
        readILMethodRef reader
        ignoreList ignoreType reader
        ignoreList ignoreType reader
        ignoreList ignoreType reader
    | 19 ->
        // TOp.Array
        ()
    | 20 ->
        // TOp.While
        ()
    | 21 ->
        // TOp.For
        reader.IgnorePackedInt()
    | 22 ->
        // TOp.Bytes
        ignoreBytes reader
    | 23 ->
        // TOp.TryCatch
        ()
    | 24 ->
        // TOp.TryFinally
        ()
    | 25 ->
        // TOp.ValFieldGetAddr
        readRecordFieldRef reader
    | 26 ->
        // TOp.UInt16s
        ignoreList (fun r -> r.ReadInt16()) reader
    | 27 ->
        // TOp.Reraise
        ()
    | 28 ->
        // TOp.UnionCaseFieldGetAddr
        ignoreUnionCaseRef reader
        reader.IgnorePackedInt()
    | 29 ->
        // TOp.Tuple
        ()
    | 30 ->
        // TOp.TupleFieldGet
        reader.IgnorePackedInt()
    | 31 ->
        // TOp.AnonRecd info
        ignoreAnonRecordInfo reader
    | 32 ->
        // TOp.AnonRecdGet
        ignoreAnonRecordInfo reader
        reader.IgnorePackedInt()

    | _ -> failwith "u_op"

and readILMethodRef (reader: FSharpMetadataReader) =
    ignoreILTypeRef reader
    ignoreCallConv reader
    reader.IgnorePackedInt() // generic arity
    reader.FindUniqueString reader |> ignore // name
    ignoreList ignoreType reader // arg types
    ignoreType reader // return type

and ignoreNonLocalValRef (reader: FSharpMetadataReader) =
    ignoreTypeRef reader
    ignoreOption reader.FindUniqueString reader
    ignoreBool reader
    reader.FindUniqueString reader |> ignore
    reader.IgnorePackedInt()
    ignoreOption ignoreType reader

and ignoreValRef (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> reader.IgnorePackedInt() // VRefLocal
    | 1 -> ignoreNonLocalValRef reader // VRefNonLocal
    | n -> failwithf "valRef: %d" n

and ignoreTraitConstraintSolution (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        // ILMethSln
        ignoreType reader
        ignoreOption ignoreILTypeRef reader // extension member
        readILMethodRef reader // solution
        ignoreList ignoreType reader // substitution

    | 1 ->
        // FSMethSln
        ignoreType reader
        ignoreValRef reader
        ignoreList ignoreType reader

    | 2 ->
        // BuiltInSln
        ()

    | 3 ->
        // ClosedExprSln // todo
        ()

    | 4 ->
        // FSRecdFieldSln
        ignoreList ignoreType reader // substitution
        readRecordFieldRef reader |> ignore
        reader.IgnoreBool() // is set of record field

    | 5 ->
        // FSAnonRecdFieldSln
        ignoreAnonRecordInfo reader
        ignoreList ignoreType reader
        reader.IgnorePackedInt() // field index

    | n -> failwithf "traitConstraintSolution: %d" n

and ignoreTraitConstraint (reader: FSharpMetadataReader) =
    ignoreList ignoreType reader
    reader.FindUniqueString reader |> ignore // id string id
    ignoreMemberFlags reader
    ignoreList ignoreType reader
    ignoreOption ignoreType reader
    ignoreOption ignoreTraitConstraintSolution reader

and readTyparConstraint (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        // TyparConstraint.CoercesTo
        ignoreType reader
    | 1 ->
        // TyparConstraint.MayResolveMember
        ignoreTraitConstraint reader
    | 2 ->
        // TyparConstraint.DefaultsTo
        ignoreType reader
    | 3 -> () // TyparConstraint.SupportsNull
    | 4 -> () // TyparConstraint.IsNonNullableStruct
    | 5 -> () // TyparConstraint.IsReferenceType
    | 6 -> () // TyparConstraint.RequiresDefaultConstructor
    | 7 ->
        // TyparConstraint.SimpleChoice
        ignoreList ignoreType reader 
    | 8 ->
        // TyparConstraint.IsEnum
        ignoreType reader
    | 9 ->
        // TyparConstraint.IsDelegate
        ignoreType reader; ignoreType reader
    | 10 -> () // TyparConstraint.SupportsComparison
    | 11 -> () // TyparConstraint.SupportsEquality
    | 12 -> () // TyparConstraint.IsUnmanaged
    | n -> failwithf "typarConstraint: %d" n

and ignoreUnionCaseRef (reader: FSharpMetadataReader) =
    ignoreTypeRef reader
    reader.FindUniqueString reader |> ignore

and readValRefFlags (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 3 -> ignoreType reader
    | _ -> ()

and ignoreDecisionTree (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        // TDSwitch
        ignoreExpr reader
        ignoreList ignoreDecisionTreeCase reader
        ignoreOption ignoreDecisionTree reader
    | 1 ->
        // TDSuccess
        ignoreList ignoreExpr reader
        reader.IgnorePackedInt()
    | 2 ->
        // TDBind
        ignoreBind reader
        ignoreDecisionTree reader
    | _ -> 
        failwith "u_dtree"

and ignoreDecisionTreeCase (reader: FSharpMetadataReader) =
    ignoreDecisionTreeDiscriminator reader
    ignoreDecisionTree reader

and ignoreDecisionTreeDiscriminator (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        // DecisionTreeTest.UnionCase
        ignoreUnionCaseRef reader
        ignoreList ignoreType reader
    | 1 ->
        // DecisionTreeTest.Const
        ignoreConst reader
    | 2 ->
        // DecisionTreeTest.IsNull
        ()
    | 3 ->
        // DecisionTreeTest.IsInst
        ignoreType reader
        ignoreType reader
    | 4 ->
        // DecisionTreeTest.ArrayLength
        reader.IgnorePackedInt()
        ignoreType reader
    | _ -> failwith "u_dtree_discrim"


and readAttributeKind (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> readILMethodRef reader
    | 1 -> ignoreValRef reader
    | _ -> failwith "u_attribkind"

and readAttributeExpr (reader: FSharpMetadataReader) =
    ignoreExpr reader // source
    ignoreExpr reader // evaluated

and readAttributeArg (reader: FSharpMetadataReader) =
    reader.FindUniqueString reader |> ignore
    ignoreType reader
    reader.IgnoreBool()
    readAttributeExpr reader

and ignoreAttribute (reader: FSharpMetadataReader) =
    ignoreTypeRef reader
    readAttributeKind reader
    ignoreList readAttributeExpr reader
    ignoreList readAttributeArg reader
    reader.IgnoreBool()

and readMethod (reader: FSharpMetadataReader) =
    ignoreSlotSig reader
    ignoreList ignoreAttribute reader
    ignoreList ignoreTypar reader
    ignoreList ignoreVal reader
    ignoreExpr reader

and readInterfaceImpl (reader: FSharpMetadataReader) =
    ignoreType reader
    ignoreList readMethod reader

and ignoreTypar (reader: FSharpMetadataReader) =
    reader.IgnorePackedInt() // typar id

    let id = readIdentAsString reader
    ignoreList ignoreAttribute reader
    reader.ReadInt64() |> ignore
    ignoreList readTyparConstraint reader
    ignoreList reader.FindUniqueString reader // xml doc id

and ignoreBind (reader: FSharpMetadataReader) =
    ignoreVal reader
    ignoreExpr reader

and readTarget (reader: FSharpMetadataReader) =
    ignoreList ignoreVal reader
    ignoreExpr reader

and readTargets (reader: FSharpMetadataReader) =
    ignoreList readTarget reader

and readStaticOptimizationConstraint (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        ignoreType reader
        ignoreType reader
    | 1 ->
        ignoreType reader
    | n ->
        failwithf "staticOptimizationConstraint: %d" n

and readSlotParam (reader: FSharpMetadataReader) =
    ignoreOption reader.FindUniqueString reader
    ignoreType reader
    reader.IgnoreBool()
    reader.IgnoreBool()
    reader.IgnoreBool()
    ignoreList ignoreAttribute reader

and ignoreSlotSig (reader: FSharpMetadataReader) =
    reader.FindUniqueString reader |> ignore // name
    ignoreType reader // declaring type
    ignoreList ignoreTypar reader // type type parameters
    ignoreList ignoreTypar reader // method type parameters
    ignoreList (ignoreList readSlotParam) reader // slot parameters
    ignoreOption ignoreType reader // return type

and ignoreMemberInfo (reader: FSharpMetadataReader) =
    ignoreTypeRef reader // apparent enclosing entity
    ignoreMemberFlags reader // member flags
    ignoreList ignoreSlotSig reader // implemented signatures
    reader.IgnoreBool() // is implemented 

and ignoreVal (reader: FSharpMetadataReader) =
    reader.IgnorePackedInt() // val id
    let logicalName = reader.FindUniqueString reader
    let compiledName = readOption reader.FindUniqueString reader
    ignoreOption ignoreRanges reader
    ignoreType reader
    reader.ReadInt64() |> ignore // val flags
    ignoreOption ignoreMemberInfo reader
    ignoreList ignoreAttribute reader
    ignoreOption ignoreValReprInfo reader
    reader.FindUniqueString reader |> ignore
    ignoreAccess reader
    ignoreParentRef reader
    ignoreOption ignoreConst reader
    ignoreUsedSpace (ignoreList reader.FindUniqueString) reader

and ignoreArgReprInfo (reader: FSharpMetadataReader) =
    ignoreList ignoreAttribute reader
    ignoreOption ignoreIdent reader

and ignoreTyparReprInfo (reader: FSharpMetadataReader) =
    ignoreIdent reader
    ignoreTypeKind reader

and ignoreValReprInfo (reader: FSharpMetadataReader) =
    ignoreList ignoreTyparReprInfo reader
    ignoreList (ignoreList ignoreArgReprInfo) reader
    ignoreArgReprInfo reader

and ignoreParentRef (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> () // ParentNone
    | 1 -> ignoreTypeRef reader // Parent
    | n -> failwithf "parentRef: %d" n

let readTypeAugmentation (reader: FSharpMetadataReader) =
    ignoreOption (ignoreTuple2 ignoreValRef ignoreValRef) reader
    ignoreOption ignoreValRef reader
    ignoreOption (ignoreTuple3 ignoreValRef ignoreValRef ignoreValRef) reader
    ignoreOption (ignoreTuple2 ignoreValRef ignoreValRef) reader
    ignoreList (ignoreTuple2 reader.FindUniqueString ignoreValRef) reader
    ignoreList (ignoreTuple2 ignoreType ignoreBool) reader
    ignoreOption ignoreType reader
    ignoreBool reader
    ignoreSpace 1 reader

let ignoreListWithExtraValue extra f (reader: FSharpMetadataReader) =
    let n = reader.ReadPackedInt()
    if n &&& 0x80000000 = 0x80000000 then
        extra reader

    for i = 0 to n - 1 do
        f reader |> ignore

let ignoreRecordField (reader: FSharpMetadataReader) =
    ignoreBool reader // mutable
    ignoreBool reader // volatile
    ignoreType reader // field type
    ignoreBool reader // static
    ignoreBool reader // secret
    ignoreOption ignoreConst reader // literal value
    ignoreIdent reader // name
    ignoreListWithExtraValue (ignoreList reader.FindUniqueString) ignoreAttribute reader 
    ignoreList ignoreAttribute reader // backing field attributes
    reader.FindUniqueString reader |> ignore // xml doc id
    ignoreAccess reader //

let ignoreExceptionTypeRepr (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> ignoreTypeRef reader // TExnAbbrevRepr
    | 1 -> ignoreILTypeRef reader // TExnAsmRepr
    | 2 -> ignoreList ignoreRecordField reader // TExnFresh
    | 3 -> () // TExnNone
    | n -> failwithf "exceptionTypeRepr: %d" n

let ignoreUnionCase (reader: FSharpMetadataReader) =
    ignoreList ignoreRecordField reader
    ignoreType reader
    reader.FindUniqueString reader |> ignore
    ignoreIdent reader
    ignoreListWithExtraValue (ignoreList reader.FindUniqueString) ignoreAttribute reader
    reader.FindUniqueString reader |> ignore
    ignoreAccess reader

let ignoreArrayBounds (reader: FSharpMetadataReader) =
    ignoreOption readInt reader
    ignoreOption readInt reader

let ignoreILArrayShape (reader: FSharpMetadataReader) =
    ignoreList ignoreArrayBounds reader

let ignoreILCallSig (reader: FSharpMetadataReader) =
    ignoreCallConv reader
    ignoreList ignoreType reader
    ignoreTypar reader

let rec ignoreILType (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> () // ILType.Void

    | 1 ->
        // ILType.Array
        ignoreILArrayShape reader
        ignoreILType reader

    | 2 -> ignoreILTypeSpec reader // ILType.Value
    | 3 -> ignoreILTypeSpec reader // mkILBoxedType
    | 4 -> ignoreILType reader // ILType.Ptr
    | 5 -> ignoreILType reader // ILType.Byref
    | 6 -> ignoreILCallSig reader // ILType.FunctionPointer
    | 7 -> reader.ReadInt16() |> ignore // ILType.TypeVar

    | 8 ->
        // ILType.Modified
        ignoreBool reader
        ignoreILTypeRef reader
        ignoreILType reader

    | n -> failwithf "ilType: %d" n

and ignoreILTypeSpec (reader: FSharpMetadataReader) =
    ignoreTypeRef reader // type ref
    ignoreList ignoreILType reader // substitution

let ignoreTypeObjectModelKind (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 -> () // TTyconClass
    | 1 -> () // TTyconInterface
    | 2 -> () // TTyconStruct
    | 3 -> ignoreSlotSig reader // TTyconDelegate
    | 4 -> () // TTyconEnum
    | n -> failwithf "typeObjectModelKind: %d" n

/// Returns true when additional reading of provided generated type should be done later.
let ignoreTypeRepr (reader: FSharpMetadataReader) =
    match reader.ReadByteAsInt() with
    | 0 ->
        // TNoRepr
        false

    | 1 ->
        match reader.ReadByteAsInt() with
        | 0 ->
            // TRecdRepr
            ignoreList ignoreRecordField reader
            false

        | 1 ->
            // TUnionRepr
            ignoreList ignoreUnionCase reader
            false

        | 2 ->
            // TAsmRepr (possibly for generated provided type which is indicated by flag that is read later)
            ignoreILType reader
            true

        | 3 ->
            // TFSharpObjectRepr
            ignoreTypeObjectModelKind reader
            ignoreList ignoreValRef reader
            ignoreList ignoreRecordField reader
            false

        | 4 ->
            // TMeasurableRepr
            ignoreType reader
            false

        | n -> failwithf "typeRepr: %d" n
    | n -> failwithf "typeRepr: %d" n

let rec ignoreModuleType (reader: FSharpMetadataReader) =
    ignoreIsType reader
    ignoreList ignoreVal reader
    ignoreList readEntity reader

and ignoreModuleTypeWrapper (reader: FSharpMetadataReader) =
    for i in 1 .. 7 do
        reader.ReadInt32() |> ignore
    ignoreModuleType reader

and readEntity (reader: FSharpMetadataReader) =
    reader.IgnorePackedInt() // entity id

    ignoreList ignoreTypar reader

    let logicalName = reader.FindUniqueString reader
    let compiledName = readOption reader.FindUniqueString reader
    ignoreRange reader

    ignoreOption readInt reader
    ignoreAccess reader
    ignoreAccess reader
    ignoreList ignoreAttribute reader
    ignoreTypeRepr reader |> ignore // here's returned flag for generated provided type
    ignoreOption ignoreType reader // abbreviation
    readTypeAugmentation reader
    reader.FindUniqueString reader |> ignore
    ignoreTypeKind reader
    reader.ReadInt64() |> ignore // is generated provided type flag
    ignoreOption ignoreCompilationPath reader
    ignoreModuleTypeWrapper reader
    ignoreExceptionTypeRepr reader
    ignoreUsedSpace (ignoreList reader.FindUniqueString) reader

    let _ =
        logicalName,
        compiledName
    
    ()

let readModuleOrNamespace (reader: FSharpMetadataReader) =
    readEntity reader

    reader.FindUniqueString reader |> ignore // compile time working dir string id
    reader.IgnorePackedInt() // uses quotations
    ignoreNBytes 3 reader // reserved space

let readStream (stream: Stream) =
    use reader = new FSharpMetadataReader(stream, Encoding.UTF8)

    // F# compiler uses "ccu" term for cross-compilation unit.
    let ccuRefNames =
        let ccuRefsNumber = reader.ReadPackedInt()
        let res = Array.zeroCreate ccuRefsNumber

        for i = 0 to ccuRefsNumber - 1 do
            match reader.ReadByteAsInt() with
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

    reader.Strings <- readArray readString reader

    ignoreList (ignoreList readInt) reader // u_encoded_pubpath
    ignoreList (ignoreIntAndIgnoreList readInt) reader // u_encoded_nleref
    ignoreList readInt reader // u_encoded_simpletyp

    reader.IgnorePackedInt()
    readModuleOrNamespace reader

    let _ =
        ccuRefNames,
        typeDeclsNumber,
        typeParameterDeclsNumber,
        valueDeclsNumber,
        anonRecordDeclsNumber,
        reader.Strings

    ()

let getFSharpSignatureInfos (psiModule: IPsiModule) =
    match psiModule.As<IAssemblyPsiModule>() with
    | null -> null
    | assemblyPsiModule ->

    let psiAssembly = assemblyPsiModule.Assembly
    let path = psiAssembly.Location
    if isNull path then null else

    Assertion.Assert(isFSharpAssembly psiModule, "isFSharpAssembly psiModule")

    use metadataLoader = new MetadataLoader()
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