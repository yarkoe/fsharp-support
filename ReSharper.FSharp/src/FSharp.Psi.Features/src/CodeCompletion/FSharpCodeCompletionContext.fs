namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.CodeCompletion

open JetBrains.ReSharper.Feature.Services.CodeCompletion.Infrastructure
open JetBrains.ReSharper.Feature.Services.Lookup
open JetBrains.ReSharper.Plugins.FSharp.Services.Cs.CodeCompletion

type FSharpItemsProviderBase() =
    inherit ItemsProviderOfSpecificContext<FSharpCodeCompletionContext>()

    // todo: override IsAvailable when it's possible to disable smart completion on the second invocation
    // override x.IsAvailable = context.BasicContext.CodeCompletionType = CodeCompletionType.BasicCompletion

    override x.GetDefaultRanges(context) = context.Ranges
    override x.GetLookupFocusBehaviour(context) = LookupFocusBehaviour.Soft