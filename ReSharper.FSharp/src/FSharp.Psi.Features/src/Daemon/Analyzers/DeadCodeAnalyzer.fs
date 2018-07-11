namespace JetBrains.ReSharper.Plugins.FSharp.Daemon.Analyzers

open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Daemon.Highlightings
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree

[<ElementProblemAnalyzer(typeof<FSharpDeadCodeToken>, HighlightingTypes = [| typeof<DeadCodeHighlighting> |] )>]
type DeadCodeAnalyzer() =
    inherit ElementProblemAnalyzer<FSharpDeadCodeToken>()

    override x.Run(element, data, consumer) =
        consumer.AddHighlighting(DeadCodeHighlighting(element.GetHighlightingRange()))
