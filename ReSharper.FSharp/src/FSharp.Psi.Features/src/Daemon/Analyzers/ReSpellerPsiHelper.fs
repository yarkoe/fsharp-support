namespace JetBrains.ReSharper.Plugins.FSharp.Daemon.Analyzers

open JetBrains.ReSharper.Features.ReSpeller.Analyzers
open JetBrains.ReSharper.Plugins.FSharp.Psi
open JetBrains.ReSharper.Psi

[<Language(typeof<FSharpLanguage>)>]
type ReSpellerPsiHelper() =
    inherit PsiHelperBase()

    override x.ShouldSkipDeclaration(declaration) =
        true // until we have rename working
