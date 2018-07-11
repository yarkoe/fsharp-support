using System;
using JetBrains.Application.Threading;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Plugins.FSharp.Psi.Tree;

namespace JetBrains.ReSharper.Plugins.FSharp.Daemon.Cs.Stages
{
  public abstract class FSharpDaemonStageProcessBase : IDaemonStageProcess
  {
    private const int InterruptCheckTime = 20;
    protected readonly SeldomInterruptCheckerWithCheckTime SeldomInterruptChecker;
    public IFSharpFile FSharpFile;

    protected FSharpDaemonStageProcessBase(IFSharpFile fsFile, IDaemonProcess daemonProcess)
    {
      FSharpFile = fsFile;
      DaemonProcess = daemonProcess;
      SeldomInterruptChecker = new SeldomInterruptCheckerWithCheckTime(InterruptCheckTime);
    }

    public IDaemonProcess DaemonProcess { get; }
    public abstract void Execute(Action<DaemonStageResult> committer);
  }
}