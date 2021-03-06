namespace JetBrains.ReSharper.Plugins.FSharp.Tests.Features

open JetBrains.ReSharper.FeaturesTestFramework.Intentions
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.QuickFixes
open JetBrains.ReSharper.Plugins.FSharp.Tests.Common
open NUnit.Framework

[<FSharpTest>]
type RemoveRedundantAttributeParensTest() =
    inherit QuickFixTestBase<RemoveRedundantAttributeParensFix>()

    override x.RelativeTestDataPath = "features/quickFixes/removeRedundantAttributeParens"

    [<Test>] member x.``Type 01 - Single attribute``() = x.DoNamedTest()
    [<Test>] member x.``Type 02 - Multiple attributes``() = x.DoNamedTest()
    [<Test>] member x.``Type 03 - Target attribute``() = x.DoNamedTest()
