module EqFix.REPL

open EqFix.Util
open EqFix.Transformer
open EqFix.RuleLib

open FSharp.Data
open Argu
open System

type TrainOptions =
    | [<MainCommand; Mandatory>] TrainFile of file: string
    | [<AltCommandLine("-only")>] Train_Only of ids: int list
    | [<AltCommandLine("-n")>] Num_Training_Example of n: int
    | [<AltCommandLine("-k")>] Max_K of k: int
    | [<AltCommandLine("-s")>] Solver of SolverName
with interface IArgParserTemplate with
        member s.Usage =
            match s with
            | TrainFile _ -> "data file"
            | Train_Only _ -> "specify a list of example groups (by ids) expected to run"
            | Num_Training_Example _ -> "for each group, take the first k examples for learning, default 1"
            | Max_K _ -> "synthesize top k programs, default 1"
            | Solver _ -> "specify the string transformer synthesizer, default stlang"

type TestOptions =
    | [<MainCommand; Mandatory>] TestFile of file: string
    | [<AltCommandLine("-only")>] Test_Only of ids: int list
    | [<AltCommandLine("-i")>] Test_Index of n: int
with interface IArgParserTemplate with
        member s.Usage =
            match s with
            | TestFile _ -> "data file"
            | Test_Only _ -> "specify a list of example groups (by ids) expected to run"
            | Test_Index _ -> "for each group, take the i-th example from last for testing, default 1"

type LoadSaveOptions =
    | [<MainCommand; Mandatory>] LibFile of file: string
with interface IArgParserTemplate with
        member s.Usage =
            match s with
            | LibFile _ -> "rule lib file"

type REPLOptions =
    | [<CliPrefix(CliPrefix.None)>] Train of ParseResults<TrainOptions>
    | [<CliPrefix(CliPrefix.None)>] Test of ParseResults<TestOptions>
    | [<CliPrefix(CliPrefix.None)>] Load of ParseResults<LoadSaveOptions>
    | [<CliPrefix(CliPrefix.None)>] Save of ParseResults<LoadSaveOptions>
    | [<AltCommandLine("q")>] Quit
with interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Train _ -> "learn new rules by examples"
            | Test _ -> "test examples"
            | Load _ -> "load rules from a rule lib"
            | Save _ -> "save rules to a rule lib"
            | Quit -> "quit"

let loadExampleGroups (file: string) (idFilter: int -> bool) =
    let all =
        [for benchmark in JsonValue.Load file ->
            (benchmark.["id"].AsInteger(),
             [for e in benchmark.["examples"] -> (e.["eq"].AsString(), e.["err"].AsString(), e.["fix"].AsString())]
            )
        ]
    List.filter (fst >> idFilter) all

// the default ID filter
let noFilter : int -> bool = fun _ -> true

let train (r: ParseResults<TrainOptions>) learner =
    let config = { maxK = r.GetResult (Max_K, defaultValue = 1);
                   solver = r.GetResult (Solver, defaultValue = SolverName.STLang) }
    Log.Debug("Solver config: {0}", config)
    let file = r.GetResult TrainFile
    let numTrainingExamples = r.GetResult (Num_Training_Example, defaultValue = 1)
    let idFilter =
        match r.TryGetResult Train_Only with
        | Some ids -> (fun i -> List.contains i ids)
        | None -> noFilter
    let data = loadExampleGroups file idFilter
    let total = data.Length
    [for (i, es) in data -> es |> (List.splitAt numTrainingExamples >> fst >> learner config)] |> ignore

let test (r: ParseResults<TestOptions>) tester =
    let file = r.GetResult TestFile
    let index = r.GetResult (Test_Index, defaultValue = 1)
    let idFilter =
        match r.TryGetResult Test_Only with
        | Some ids -> (fun i -> List.contains i ids)
        | None -> noFilter
    let data = loadExampleGroups file idFilter
    let total = data.Length
    [for (i, es) in data ->
        Log.Info("Running benchmark #{0} of {1}", i, total)
        match List.rev es |> List.item (index - 1) |> tester with
        | Some (k: int) -> Log.Info("Success: {0} rule(s) attempted", k)
        | None -> Log.Info("Failure: no rule applicable")
    ] |> ignore

let repl (it: Iterator<string>) =
    let errorHandler = ExceptionExiter()
    let parser: ArgumentParser<REPLOptions> = ArgumentParser.Create(programName = ">", errorHandler = errorHandler)
    printfn "EqFix interactive REPL"

    // set up rule lib
    let ruleLib = RuleLib ()

    // loop
    let mutable continues = true
    let emptyResult = parser.ParseCommandLine [| |]
    while continues && it.HasCurrent do
        printf "> "
        let input = it.GetCurrent
        it.MoveNext()
        let results =
            try parser.ParseCommandLine (input.Split ' ') with
            | :? ArguParseException as ex ->
                // NOTE: mandatory arg causes uncaught exception
                let writer = if ex.ErrorCode = ErrorCode.HelpText then Console.Out else Console.Error
                writer.WriteLine ex.Message
                writer.Flush()
                emptyResult

        let handler =
            function
            | Train r -> train r ruleLib.Learn
            | Test r -> test r ruleLib.Test
            | Load r -> r.GetResult LibFile |> ruleLib.Load
            | Save r -> r.GetResult LibFile |> ruleLib.Dump
            | Quit -> continues <- false

        results.GetAllResults() |> List.iter handler
