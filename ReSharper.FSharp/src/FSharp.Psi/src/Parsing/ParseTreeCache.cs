using System;
using FSharp.Compiler.SourceCodeServices;
using JetBrains.Lifetimes;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Plugins.FSharp.Checker;
using JetBrains.ReSharper.Psi;
using JetBrains.Util.Caches;
using Microsoft.FSharp.Core;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Parsing
{
  [SolutionComponent]
  public class ParseTreeCache
  {

    public LRUWeakRefRetainerCache<FSharpOption<FSharpParseFileResults>> Cache;
    public readonly Func<IPsiSourceFile, FSharpOption<FSharpParseFileResults>> ParseFunc;

    public ParseTreeCache(Lifetime lifetime, FSharpCheckerService checkerService)
    {
      Cache = new LRUWeakRefRetainerCache<FSharpOption<FSharpParseFileResults>>(lifetime, 5);
      ParseFunc = checkerService.ParseFile;
    }
  }
}
