namespace JetBrains.ReSharper.Plugins.FSharp.Daemon.Stages

open JetBrains.Application.Settings
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Daemon.Cs.Stages
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.Util
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.SourceCodeServices

type TypeCheckErrorsStageProcess(fsFile: IFSharpFile, daemonProcess, logger: ILogger) =
    inherit ErrorsStageProcessBase(fsFile, daemonProcess)

    override x.ShouldAddDiagnostic(error, range) =
        error.Subcategory <> BuildPhaseSubcategory.Parse &&
        base.ShouldAddDiagnostic(error, range)

    override x.Execute(committer) =
        match fsFile.GetParseAndCheckResults(false) with
        | Some results ->
            daemonProcess.CustomData.PutData(FSharpDaemonStageBase.TypeCheckResults, Some results.CheckResults)
            let projectWarnings, fileErrors  =
                results.CheckResults.Errors
                |> Array.partition (fun e ->
                    e.StartLineAlternate = 0 && e.EndLineAlternate = 0 && e.Severity = FSharpErrorSeverity.Warning)

            if not (Array.isEmpty projectWarnings) then
                // https://github.com/Microsoft/visualfsharp/issues/4030
                let errors = Array.fold (fun a e -> a + "\n" + e.ToString()) "" projectWarnings
                logger.LogMessage(LoggingLevel.WARN, "Project warnings during file type check:" + errors)
            x.Execute(fileErrors, committer)
        | _ -> ()


[<DaemonStage(StagesBefore = [| typeof<SyntaxErrorsStage> |], StagesAfter = [| typeof<HighlightIdentifiersStage> |])>]
type TypeCheckErrorsStage(logger: ILogger) =
    inherit FSharpDaemonStageBase()

    override x.CreateProcess(fsFile: IFSharpFile, settings: IContextBoundSettingsStore, daemonProcess: IDaemonProcess) =
        TypeCheckErrorsStageProcess(fsFile, daemonProcess, logger) :> IDaemonStageProcess
