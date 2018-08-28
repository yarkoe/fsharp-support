using JetBrains.Application.changes;
using JetBrains.DataFlow;

namespace JetBrains.ReSharper.Plugins.FSharp
{
  public abstract class ChangeListenerBase
  {
    protected ChangeListenerBase(Lifetime lifetime, ChangeManager changeManager)
    {
      changeManager.Changed2.Advise(lifetime, Execute);
    }

    protected virtual void Execute(ChangeEventArgs obj)
    {
    }
  }
}
