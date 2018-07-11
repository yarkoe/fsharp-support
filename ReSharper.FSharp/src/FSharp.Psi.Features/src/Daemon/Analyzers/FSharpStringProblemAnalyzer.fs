namespace JetBrains.ReSharper.Plugins.FSharp.Daemon.Analyzers

open JetBrains.ReSharper.Daemon.StringAnalysis
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Util
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Parsing
open JetBrains.ReSharper.Psi.Tree
open JetBrains.Text
open JetBrains.Util

[<ElementProblemAnalyzer(typeof<FSharpString>)>]
type FSharpStringProblemAnalyzer() =
    inherit StringProblemAnalyzerBase<FSharpString>()

    static let stringLexerKey = Key<CachedPsiValue<IStringLexer>>("CachedFSharpLiteralWrapper")

    let createLexer (literalToken: FSharpString): IStringLexer  =
        let literalType = literalToken.GetTokenType().GetLiteralType()
        let buffer = StringBuffer(literalToken.GetText())

        match literalType with
        | FSharpLiteralType.Character
        | FSharpLiteralType.RegularString -> RegularStringLexer(buffer) :> _
        | FSharpLiteralType.VerbatimString -> VerbatimStringLexer(buffer) :> _
        | FSharpLiteralType.TripleQuoteString -> TripleQuoteStringLexer(buffer) :> _
        | FSharpLiteralType.ByteArray -> ByteArrayStringLexer(buffer) :> _

    let getCachedLexer (literalToken: FSharpString) =
        let isValid = literalToken.IsValid()
        if not isValid then createLexer literalToken else

        match literalToken.UserData.GetData(stringLexerKey) with
        | null ->
            let cachedValue = CachedPsiValue()
            let lexer = createLexer literalToken
            cachedValue.SetValue(literalToken, lexer)
            literalToken.UserData.PutData(stringLexerKey, cachedValue)
            lexer

        | cachedValue ->
            match cachedValue.GetValue(literalToken) with
            | null -> 
                let lexer = createLexer literalToken
                cachedValue.SetValue(literalToken, lexer)
                lexer

            | lexer -> lexer

    override x.ExtractElements(literalToken: FSharpString, consumer: IHighlightingConsumer) =
        let lexer = getCachedLexer literalToken
        [| Pair(literalToken :> ITokenNode, lexer) |] :> _
