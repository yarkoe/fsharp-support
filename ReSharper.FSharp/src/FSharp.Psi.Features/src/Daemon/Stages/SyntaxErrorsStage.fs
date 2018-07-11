namespace JetBrains.ReSharper.Plugins.FSharp.Daemon.Stages

open JetBrains.Application.Settings
open JetBrains.ReSharper.Daemon.Stages
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Daemon.Cs.Stages
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree

type SyntaxErrorsStageProcess(fsFile: IFSharpFile, daemonProcess) =
    inherit ErrorsStageProcessBase(fsFile, daemonProcess)

    override x.Execute(committer) =
        match fsFile.CheckerService.ParseFile(daemonProcess.SourceFile) with
        | Some parseResults -> x.Execute(parseResults.Errors, committer)
        | _ -> ()


[<DaemonStage(StagesBefore = [| typeof<GlobalFileStructureCollectorStage> |],
              StagesAfter  = [| typeof<HighlightIdentifiersStage> |])>]
type SyntaxErrorsStage(daemonProcess, errors) =
    inherit FSharpDaemonStageBase()

    override x.CreateProcess(fsFile: IFSharpFile, settings: IContextBoundSettingsStore, daemonProcess: IDaemonProcess) =
        SyntaxErrorsStageProcess(fsFile, daemonProcess) :> IDaemonStageProcess
