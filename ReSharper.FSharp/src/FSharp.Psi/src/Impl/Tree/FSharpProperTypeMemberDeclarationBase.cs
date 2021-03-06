﻿using System;
using System.Diagnostics;
using JetBrains.Annotations;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Caches2;
using JetBrains.ReSharper.Psi.Modules;
using JetBrains.ReSharper.Psi.Tree;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Impl.Tree
{
  public abstract class FSharpProperTypeMemberDeclarationBase : FSharpTypeMemberDeclarationBase,
    ICachedTypeMemberDeclaration
  {
    [CanBeNull]
    protected abstract IDeclaredElement CreateDeclaredElement();

    private static readonly Func<FSharpProperTypeMemberDeclarationBase, IDeclaredElement>
      DeclaredElementFactory = declaration => declaration.CreateDeclaredElement();

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public override IDeclaredElement DeclaredElement
    {
      get
      {
        this.AssertIsValid("Asking declared element from invalid declaration");
        var cache = GetPsiServices().Caches.SourceDeclaredElementsCache;
        // todo: calc types on demand in members (move cookie to FSharpTypesUtil)
        using (CompilationContextCookie.GetOrCreate(GetPsiModule().GetContextFromModule()))
          return cache.GetOrCreateDeclaredElement(this, DeclaredElementFactory);
      }
    }
  }
}