namespace rec JetBrains.ReSharper.Plugins.FSharp.Daemon.Stages

open System
open JetBrains.Application.Settings
open JetBrains.ReSharper.Daemon.Stages.Dispatcher
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Daemon.Cs.Stages
open JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree

[<DaemonStage(StagesBefore = [| typeof<HighlightIdentifiersStage> |])>]
type FSharpErrorStage(daemonProcess, elementProblemAnalyzerRegistrar) =
    inherit FSharpDaemonStageBase()

    override x.CreateProcess(fsFile: IFSharpFile, settings: IContextBoundSettingsStore, daemonProcess: IDaemonProcess) =
        FSharpErrorStageProcess(fsFile, daemonProcess, settings, elementProblemAnalyzerRegistrar) :> IDaemonStageProcess


type FSharpErrorStageProcess
        (fsFile: IFSharpFile, daemonProcess: IDaemonProcess, settings: IContextBoundSettingsStore,
         analyzerRegistrar: ElementProblemAnalyzerRegistrar) =
    inherit TreeNodeVisitor<IHighlightingConsumer>()

    static let analyzerRunKind = ElementProblemAnalyzerRunKind.FullDaemon

    let interruptCheck = daemonProcess.GetCheckForInterrupt()
    let elementProblemAnalyzerData = ElementProblemAnalyzerData(fsFile, settings, analyzerRunKind, interruptCheck)
    let analyzerDispatcher = analyzerRegistrar.CreateDispatcher(elementProblemAnalyzerData)

    member x.DaemonProcess = daemonProcess

    override x.VisitNode(element: ITreeNode, consumer: IHighlightingConsumer) =
        analyzerDispatcher.Run(element, consumer)

    interface IDaemonStageProcess with
        member x.Execute(committer) =
            let consumer = FilteringHighlightingConsumer(daemonProcess.SourceFile, fsFile, settings)
            fsFile.ProcessThisAndDescendants(Processor(x, consumer))
            committer.Invoke(DaemonStageResult(consumer.Highlightings))

        member x.DaemonProcess = x.DaemonProcess


type Processor(daemonProcess: FSharpErrorStageProcess, consumer: IHighlightingConsumer) =
    abstract InteriorShouldBeProcessed: ITreeNode -> bool
    default x.InteriorShouldBeProcessed(element) = true

    interface IRecursiveElementProcessor with
        member x.InteriorShouldBeProcessed(element) =
            x.InteriorShouldBeProcessed(element)

        member x.ProcessingIsFinished =
            match daemonProcess.DaemonProcess.InterruptFlag with
            | true -> OperationCanceledException() |> raise
            | _ -> false
    
        member x.ProcessBeforeInterior(element) = ()

        member x.ProcessAfterInterior(element) =
            match element with
            | :? FSharpToken as token ->
                if not (token.GetTokenType().IsWhitespace) then
                    token.Accept(daemonProcess, consumer)

            | _ -> daemonProcess.VisitNode(element, consumer)
