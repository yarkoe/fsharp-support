namespace JetBrains.ReSharper.Plugins.FSharp.Common.Checker

open System
open System.Runtime.CompilerServices
open Microsoft.FSharp.Compiler.SourceCodeServices

[<AutoOpen>]
module FSharpCheckerExtensions =
    let map (f: 'T -> 'U) (a: Async<'T>) : Async<'U> =
        async {
            let! a = a
            return f a
        }

[<Extension; Sealed; AbstractClass>]
type FSharpCheckerExtensions =
    [<Extension>]
    static member ParseAndCheckDocument(checker: FSharpChecker, filePath: string, sourceText: string, options: FSharpProjectOptions, allowStaleResults: bool) =
        let timeout = 1500
        let parseAndCheckFile =
            async {
                let! parseResults, checkFileAnswer = checker.ParseAndCheckFileInProject(filePath, 0, sourceText, options)
                return
                    match checkFileAnswer with
                    | FSharpCheckFileAnswer.Aborted ->
                        None
                    | FSharpCheckFileAnswer.Succeeded(checkFileResults) ->
                        Some (parseResults, checkFileResults)
            }

        let tryGetFreshResultsWithTimeout() =
            async {
                let! worker = Async.StartChild(parseAndCheckFile, timeout)
                try
                    return! worker
                with :? TimeoutException ->
                    return None // worker is cancelled at this point, we cannot return it and wait its completion anymore
            }

        let bindParsedInput(results: (FSharpParseFileResults * FSharpCheckFileResults) option) =
            match results with
            | Some(parseResults, checkResults) ->
                match parseResults.ParseTree with
                | Some parsedInput -> Some (parseResults, parsedInput, checkResults)
                | None -> None
            | None -> None

        if allowStaleResults then
            async {
                let! freshResults = tryGetFreshResultsWithTimeout()

                let! results =
                    match freshResults with
                    | Some x -> async.Return (Some x)
                    | None ->
                        async {
                            match checker.TryGetRecentCheckResultsForFile(filePath, options) with
                            | Some (parseResults, checkFileResults, _) ->
                                return Some (parseResults, checkFileResults)
                            | None ->
                                return! parseAndCheckFile
                        }
                return bindParsedInput results
            }
        else parseAndCheckFile |> map bindParsedInput
