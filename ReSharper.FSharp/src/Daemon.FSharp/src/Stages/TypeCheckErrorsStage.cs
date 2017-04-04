﻿using JetBrains.Annotations;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.FSharp.Tree;
using JetBrains.Util;
using Microsoft.FSharp.Compiler;

namespace JetBrains.ReSharper.Daemon.FSharp.Stages
{
  [DaemonStage(StagesBefore = new[] {typeof(SetResolvedSymbolsStage)},
    StagesAfter = new[] {typeof(ResolvedIdentifiersHighlightStage)})]
  public class TypeCheckErrorsStage : FSharpDaemonStageBase
  {
    public class TypeCheckErrorsStageProcess : ErrorsStageProcess
    {
      public TypeCheckErrorsStageProcess([NotNull] IDaemonProcess process, [NotNull] FSharpErrorInfo[] errors)
        : base(process, errors)
      {
      }
    }

    protected override IDaemonStageProcess CreateProcess(IFSharpFile fsFile, IDaemonProcess process)
    {
      var errors = fsFile.GetCheckResults(false, process.CreateInterruptChecker())?.Errors;
      return new TypeCheckErrorsStageProcess(process, errors ?? EmptyArray<FSharpErrorInfo>.Instance);
    }

    public override ErrorStripeRequest NeedsErrorStripe(IPsiSourceFile sourceFile,
      IContextBoundSettingsStore settingsStore)
    {
      return ErrorStripeRequest.STRIPE_AND_ERRORS;
    }
  }
}