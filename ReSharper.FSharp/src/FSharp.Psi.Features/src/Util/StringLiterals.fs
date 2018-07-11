namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util

open System
open System.Globalization
open System.Runtime.CompilerServices
open JetBrains.ReSharper.Plugins.FSharp.Common.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
open JetBrains.ReSharper.Psi.Parsing
open JetBrains.Util

[<Extension>]
type FSharpLiteralType =
    /// '{char}'
    | Character
    /// "{string}"
    | RegularString
    /// @"{string}"
    | VerbatimString
    /// """{string}"""
    | TripleQuoteString
    /// "{string}"B
    | ByteArray

    [<Extension>]
    static member GetLiteralType(literalTokenType: TokenNodeType) =
       if literalTokenType == FSharpTokenType.CHAR then FSharpLiteralType.Character else
       if literalTokenType == FSharpTokenType.STRING then FSharpLiteralType.RegularString else
       if literalTokenType == FSharpTokenType.VERBATIM_STRING then FSharpLiteralType.VerbatimString else
       if literalTokenType == FSharpTokenType.TRIPLE_QUOTE_STRING then FSharpLiteralType.TripleQuoteString else
       if literalTokenType == FSharpTokenType.BYTEARRAY then FSharpLiteralType.ByteArray else
       InvalidOperationException("Token {literalTokenType} is not string literal") |> raise


type RegularStringLexer(buffer) =
    inherit StringLexerBase(buffer)

    static let maxUnicodeCodePoint = uint32 0x10FFFF

    override x.StartOffset = 1
    override x.EndOffset = 1

    override x.AdvanceInternal() =
        match x.Buffer.[x.Position] with
        | '\\' ->
            x.Position <- x.Position + 1
            if x.CanAdvance then x.ProcessEscapeSequence()
            else StringTokenTypes.CHARACTER
        | _ -> StringTokenTypes.CHARACTER

    abstract ProcessEscapeSequence: unit -> TokenNodeType
    default x.ProcessEscapeSequence() =
        match x.Buffer.[x.Position] with
        | 'u' -> x.ProcessHexEscapeSequence(4)
        | 'U' -> x.ProcessLongHexEscapeSequence()
        | '"' | '\'' | '\\' | 'b' | 'n' | 'r' | 't' -> StringTokenTypes.ESCAPE_CHARACTER
        | _ -> StringTokenTypes.CHARACTER

    member x.ProcessHexEscapeSequence(length) =
        let str = x.ProcessEscapeSequence(length, length, 1, (fun c -> c.IsHexDigitFast()))
        if str.Length = length then StringTokenTypes.ESCAPE_CHARACTER else StringTokenTypes.CHARACTER

    member x.ProcessLongHexEscapeSequence() =
        let hex = x.ProcessEscapeSequence(8, max = 8, shift = 1, matcher = (fun c -> c.IsHexDigitFast()))
        if hex.Length <> 8 then StringTokenTypes.CHARACTER else

        let mutable codePoint = Unchecked.defaultof<uint32>
        match UInt32.TryParse(hex, NumberStyles.HexNumber, null, &codePoint) with
        | true when codePoint <= maxUnicodeCodePoint -> StringTokenTypes.ESCAPE_CHARACTER
        | _ -> StringTokenTypes.CHARACTER

    override x.ParseEscapeCharacter(value: string) =
        match value.Length with
        | 1 -> value.[0].ToString()
        | 2 when value.[0] = '\\' ->
            match value.[1] with
            | '\\' -> "\\"
            | '\'' -> "\'"
            | '"' -> "\""
            | 'b' -> "\b"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | _ -> ArgumentException("Invalid Character") |> raise

        | n when n > 2 && value.[0] = '\\' ->
            match value.[1] with
            | 'u' -> StringLexerBase.ParseHexEscapeSequence(value, min = 4, max = 4)
            | 'U' -> StringLexerBase.ParseHexEscapeSequence(value, min = 8, max = 8)
            | _ -> ArgumentException("Invalid Character") |> raise

        | _ -> ArgumentException("Invalid Character") |> raise


type VerbatimStringLexer(buffer) =
    inherit StringLexerBase(buffer)

        override x.StartOffset = 2
        override x.EndOffset = 1

        override x.AdvanceInternal() =
            if x.Buffer.[x.Position] = '\"' then
                x.Position <- x.Position + 1

                if x.CanAdvance && x.Buffer.[x.Position] = '\"' then StringTokenTypes.ESCAPE_CHARACTER else
                StringTokenTypes.CHARACTER

            else StringTokenTypes.CHARACTER

        override x.ParseEscapeCharacter(value: string) =
            if value.Length = 1 then value.[0].ToString() else

            if value.Length = 2 then
                Assertion.Assert(value <> @"""", "Invalid character presentation");
                "\""

            else ArgumentException("Invalid character presentation") |> raise


type TripleQuoteStringLexer(buffer) =
        inherit VerbatimStringLexer(buffer)

        override x.StartOffset = 4
        override x.EndOffset = 4

        override x.AdvanceInternal() = StringTokenTypes.CHARACTER


type ByteArrayStringLexer(buffer) =
    inherit RegularStringLexer(buffer)

    override x.EndOffset = 2

    override x.ProcessEscapeSequence() =
        match x.Buffer.[x.Position] with
        | '\\' -> StringTokenTypes.ESCAPE_CHARACTER
        | _ -> StringTokenTypes.CHARACTER

    override x.ParseEscapeCharacter(value: string) =
        match value.Length with
        | 1 -> value.[0].ToString()
        | 2 when value.[0] = '\\' ->
            match value.[1] with
            | '\\' -> "\\"
            | '\'' -> "\'"
            | '"' -> "\""
            | 'b' -> "\b"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | _ -> ArgumentException("Invalid Character") |> raise
        | _ -> ArgumentException("Invalid Character") |> raise
