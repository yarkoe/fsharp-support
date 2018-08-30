using JetBrains.Application.changes;
using JetBrains.DataFlow;
using JetBrains.Util;
using static Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader;

namespace JetBrains.ReSharper.Plugins.FSharp
{
  public abstract class AssemblyReaderShimBase : FileSystemShimChangeProvider, Shim.IAssemblyReader
  {
    private readonly Shim.IAssemblyReader myDefaultReader;

    protected AssemblyReaderShimBase(Lifetime lifetime, ChangeManager changeManager) : base(lifetime, changeManager)
    {
      myDefaultReader = Shim.AssemblyReader;
      Shim.AssemblyReader = this;
      lifetime.AddAction(() => Shim.AssemblyReader = myDefaultReader);
    }

    protected virtual ILModuleReader GetModuleReader(FileSystemPath path, ILReaderOptions readerOptions) =>
      myDefaultReader.GetILModuleReader(path.FullPath, readerOptions);

    public ILModuleReader GetILModuleReader(string filename, ILReaderOptions readerOptions)
    {
      var path = FileSystemPath.TryParse(filename);
      return !path.IsEmpty
        ? GetModuleReader(path, readerOptions)
        : myDefaultReader.GetILModuleReader(filename, readerOptions);
    }
  }
}
