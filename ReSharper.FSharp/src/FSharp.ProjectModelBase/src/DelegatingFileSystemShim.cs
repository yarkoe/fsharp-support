using System;
using System.IO;
using System.Reflection;
using JetBrains.Application.changes;
using JetBrains.DataFlow;
using JetBrains.Util;
using static Microsoft.FSharp.Compiler.AbstractIL.Internal.Library;

namespace JetBrains.ReSharper.Plugins.FSharp
{
  public abstract class FileSystemShimChangeProvider : DelegatingFileSystemShim
  {
    protected FileSystemShimChangeProvider(Lifetime lifetime, ChangeManager changeManager) : base(lifetime) =>
      changeManager.Changed2.Advise(lifetime, Execute);

    public virtual void Execute(ChangeEventArgs changeEventArgs)
    {
    }
  }

  public class DelegatingFileSystemShim : Shim.IFileSystem
  {
    public DelegatingFileSystemShim(Lifetime lifetime)
    {
      OverridenFileSystem = Shim.FileSystem;
      IsOverridingDelegating = OverridenFileSystem is DelegatingFileSystemShim;
      Shim.FileSystem = this;
      lifetime.AddAction(() => Shim.FileSystem = OverridenFileSystem);
    }

    private Shim.IFileSystem OverridenFileSystem { get; }
    private bool IsOverridingDelegating { get; }

    private DelegatingFileSystemShim OverridenDelegatingFileSystem =>
      (DelegatingFileSystemShim) OverridenFileSystem;

    public virtual bool Exists(FileSystemPath path) =>
      IsOverridingDelegating
        ? OverridenDelegatingFileSystem.Exists(path)
        : OverridenFileSystem.SafeExists(path.FullPath);

    public virtual DateTime GetLastWriteTime(FileSystemPath path) =>
      IsOverridingDelegating
        ? OverridenDelegatingFileSystem.GetLastWriteTime(path)
        : OverridenFileSystem.GetLastWriteTimeShim(path.FullPath);

    [Obsolete("Use in tests only.")]
    public virtual DateTime GetLastWriteTimeShim(string fileName)
    {
      var path = FileSystemPath.TryParse(fileName);
      return path.IsEmpty
        ? OverridenFileSystem.GetLastWriteTimeShim(fileName)
        : GetLastWriteTime(path);
    }

    [Obsolete("Use in tests only.")]
    public virtual bool SafeExists(string fileName)
    {
      var path = FileSystemPath.TryParse(fileName);
      return path.IsEmpty
        ? OverridenFileSystem.SafeExists(fileName)
        : Exists(path);
    }

    public virtual byte[] ReadAllBytesShim(string fileName) => OverridenFileSystem.ReadAllBytesShim(fileName);
    public virtual Stream FileStreamReadShim(string fileName) => OverridenFileSystem.FileStreamReadShim(fileName);
    public virtual Stream FileStreamCreateShim(string fileName) => OverridenFileSystem.FileStreamCreateShim(fileName);
    public virtual string GetFullPathShim(string fileName) => OverridenFileSystem.GetFullPathShim(fileName);
    public virtual bool IsPathRootedShim(string path) => OverridenFileSystem.IsPathRootedShim(path);
    public virtual bool IsInvalidPathShim(string filename) => OverridenFileSystem.IsInvalidPathShim(filename);
    public virtual string GetTempPathShim() => OverridenFileSystem.GetTempPathShim();
    public virtual void FileDelete(string fileName) => OverridenFileSystem.FileDelete(fileName);
    public virtual Assembly AssemblyLoadFrom(string fileName) => OverridenFileSystem.AssemblyLoadFrom(fileName);
    public virtual Assembly AssemblyLoad(AssemblyName assemblyName) => OverridenFileSystem.AssemblyLoad(assemblyName);
    public virtual bool IsStableFileHeuristic(string fileName) => OverridenFileSystem.IsStableFileHeuristic(fileName);

    public virtual Stream FileStreamWriteExistingShim(string fileName) =>
      OverridenFileSystem.FileStreamWriteExistingShim(fileName);
  }
}
