namespace JetBrains.ReSharper.Plugins.FSharp.Tests.Common.Scripts

open System
open System.Linq
open System.IO
open JetBrains.Application
open JetBrains.Application.Components
open JetBrains.Application.Environment
open JetBrains.Application.platforms
open JetBrains.DataFlow
open JetBrains.Diagnostics
open JetBrains.Lifetimes
open JetBrains.ProjectModel
open JetBrains.ProjectModel.BuildTools
open JetBrains.ProjectModel.ProjectsHost.SolutionHost
open JetBrains.ProjectModel.ProjectsHost.SolutionHost.Impl
open JetBrains.ReSharper.Host.Features.Toolset
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel.ProjectItems.ItemsContainer
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel.ProjectItems.ProjectStructure
open JetBrains.ReSharper.Plugins.FSharp.ProjectModel.Scripts
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Modules
open JetBrains.TestFramework
open JetBrains.TestFramework.Projects
open JetBrains.Util
open NUnit.Framework

type ScriptPsiModulesTest() =
    inherit BaseTest()

    override x.RelativeTestDataPath = "projectModel/scripts"

    [<Test>] member x.SolutionItem() = x.DoTestSolution()
    [<Test>] member x.FileInProject() = x.DoTestSolution()
    [<Test>] member x.MultipleTargetFrameworks() = x.DoTestSolution()

    [<Test>] member x.CSharpProject() = x.DoTestSolution()
    [<Test>] member x.FileDoNotExist() = x.DoTestSolution()

    member x.DoTestSolution() =
        x.RunGuarded(fun _ -> Lifetime.Using(x.DoTestSolutionImpl))

    member x.DoTestSolutionImpl(lifetime: Lifetime) =
        use persistCacheCookie = x.ShellInstance.GetComponent<TestCachesConfigurationSettings>().PersistCachesCookie()

        let solution = x.OpenSolution(lifetime)
        x.ExecuteWithGold(fun writer ->
            let scriptModulesProvider = solution.GetComponent<FSharpScriptPsiModulesProvider>()
            scriptModulesProvider.Dump(writer)
            x.DumpSourceFilePersistentIds(solution, writer))

    member x.OpenSolution(lifetime: Lifetime): ISolution =
        let tempSolutionPath = x.CopyTestDataDirectoryToTemp2(lifetime, x.TestMethodName)
        let solution = x.SolutionManager.OpenSolution(tempSolutionPath / x.SolutionFileName)
        lifetime.OnTermination(fun _ -> x.SolutionManager.CloseSolution()) |> ignore
        solution

    member x.DumpSourceFilePersistentIds(solution: ISolution, writer: TextWriter) =
        writer.WriteLine()
        writer.WriteLine("Source files persistent ids:")

        let psiModules = solution.PsiModules()
        for project in solution.GetTopLevelProjects() do
            for projectFile in project.GetAllProjectFiles() do
                if not (projectFile.LanguageType.Is<FSharpScriptProjectFileType>()) then () else

                writer.WriteLine(projectFile.Location.MakeRelativeTo(solution.SolutionDirectory))
                for sourceFile in psiModules.GetPsiSourceFilesFor(projectFile) do
                    writer.WriteLine("  " + sourceFile.GetPersistentID())

    member x.SolutionManager: SolutionHostManager =
        x.ShellInstance.GetComponent<SolutionHostManager>()

    member x.SolutionFileName: string =
        x.TestMethodName + ".sln"

    member x.ExecuteWithGold(action: Action<TextWriter>) =
        base.ExecuteWithGold(action) |> ignore


[<SolutionInstanceComponent>]
type MyTestSolutionToolset(lifetime: Lifetime, buildToolContainer: BuildToolContainer) =
    inherit DefaultSolutionToolset(lifetime)

    let changed = new Signal<_>(lifetime, "MySolutionToolset::Changed")

    let dotnetDirValue = Environment.GetEnvironmentVariable("DOTNET_SDK_DIRECTORY")
    let dotnetDir = FileSystemPath.TryParse(dotnetDirValue)

    let buildTool =
        if dotnetDirValue.IsNullOrEmpty() then
            buildToolContainer.GetAutoDetected(BuildToolEnvironment.EmptyEnvironment)
        else
            let dotnetDir = FileSystemPath.TryParse(dotnetDirValue)
            Logging.Logger.LogMessage(LoggingLevel.INFO, "Dotnet path: {0}", dotnetDir)
            let sdks = dotnetDir / "sdk"
            let sdkPath = sdks.GetChildDirectories().First()
            Logging.Logger.LogMessage(LoggingLevel.INFO, "SDK path: {0}", sdkPath)
            let msbuildPath = sdkPath / "MSBuild.dll"
            Assertion.Assert(msbuildPath.ExistsFile, "msbuildPath.ExistsFile")

            CustomBuildTool(sdkPath / "MSBuild.dll") :> _

    let cli =
        if dotnetDirValue.IsNullOrEmpty() then
//            buildToolContainer.GetAutoDetected(BuildToolEnvironment.EmptyEnvironment)
            null
        else
            DotNetCoreCli.Create(dotnetDir).Cli

    interface IRiderSolutionToolset with
        member x.GetBuildTool() = buildTool
        member x.Changed = changed :> _

        member x.GetMonoRuntime() = null
        member x.GetDotNetCoreToolset() = DotNetCoreToolset(cli, null)


[<ZoneActivator>]
type SolutionHostZoneActivator() =
    interface IActivate<IHostSolutionZone> with
        member x.ActivatorEnabled() = true


[<SolutionInstanceComponent>]
type FSharpProjectStructurePresenterStub() =
    interface IHideImplementation<FSharpProjectStructurePresenter>


[<SolutionInstanceComponent>]
type FSharpItemsContainerRefresherStub() =
    interface IHideImplementation<FSharpItemsContainerRefresher>

    interface  IFSharpItemsContainerRefresher with
        member x.RefreshProject(_, _) = ()
        member x.RefreshFolder(_, _, _) = ()
        member x.UpdateFile(_, _) = ()
        member x.UpdateFolder(_, _, _) = ()
        member x.ReloadProject(_) = ()
        member x.SelectItem(_, _) = ()


[<SolutionFeaturePart>]
type FSharpItemModificationContextProviderStub() =
    interface IHideImplementation<FSharpItemModificationContextProvider>

[<ShellComponent>]
type FSharpFileServiceStub() =
    interface IHideImplementation<FSharpFileService>

    interface IFSharpFileService with
        member x.IsScratchFile(_) = false
        member x.IsScriptLike(_) = false
