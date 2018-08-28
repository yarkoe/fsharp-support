using JetBrains.Application.changes;
using JetBrains.DataFlow;
using static Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader;

namespace JetBrains.ReSharper.Plugins.FSharp
{
  public class AssemblyReaderShimBase : FileSystemShimChangeProvider, Shim.IAssemblyReader
  {
    private readonly Shim.IAssemblyReader myDefaultReader;

    protected AssemblyReaderShimBase(Lifetime lifetime, ChangeManager changeManager) : base(lifetime, changeManager)
    {
      myDefaultReader = Shim.AssemblyReader;
      Shim.AssemblyReader = this;
      lifetime.AddAction(() => Shim.AssemblyReader = myDefaultReader);
    }

    public virtual ILModuleReader GetILModuleReader(string filename, ILReaderOptions readerOptions) =>
      myDefaultReader.GetILModuleReader(filename, readerOptions);
  }
}
