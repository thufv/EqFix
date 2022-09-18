open EqFix.Util
open EqFix.Synthesizer
open EqFix.Transformer
open EqFix.RuleLib
open EqFix.Lib.Logging
open EqFix

open FSharp.Data
open Argu

let parseBenchmark (file: string) (idFilter: int -> bool) =
    let all =
        [for benchmark in JsonValue.Load file ->
            (benchmark.["id"].AsInteger(),
             [for e in benchmark.["examples"] -> (e.["eq"].AsString(), e.["err"].AsString(), e.["fix"].AsString())]
            )
        ]
    List.filter (fst >> idFilter) all

// the default ID filter
let noFilter = fun _ -> true

let run config (file: string) (idFilter: int -> bool) (numTrainingExamples: int) runner =
    Log.Info("Loading benchmark: {0}", file)
    Log.Info("Number of training examples: {0}", numTrainingExamples)

    let data = parseBenchmark file idFilter
    let total = data.Length
    data |> List.iter (fun (i, es) ->
        Log.Info("Running benchmark #{0} of {1}", i, total)
        let rs = (List.take numTrainingExamples es, List.singleton (List.last es)) |> runner config
        match rs with
        | None -> Log.Info("Failure: no rule synthesized")
        | Some xs ->
            match List.last xs with
            // strange: without this type annotation, F# cannot auto convert int to object[]
            | Some (k: int) -> Log.Info("Success: {0} rule(s) attempted", k)
            | None -> Log.Info("Failure: no rule applicable")
    )

let runBenchmarksEqFix (config: SolverConfig) (file: string) 
                       (idFilter: int -> bool) (numTrainingExamples: int) =
    run config file idFilter numTrainingExamples synthesizeRulesAndTest

let runBenchmarksNaive (config: SolverConfig) (file: string) 
                       (idFilter: int -> bool) (numTrainingExamples: int) =
    let transform (examples: Example list): TExample list =
        examples ||> (fun (eq, err, fix) -> ([eq; err], fix))

    let synthesize (config: SolverConfig) (examples: TExample list): STVSA option =
        let solver = Solver config in solver.Solve examples

    let testOne (fs: STVSA) (test: TExample) : int option =
        let x, y = test
        Log.Debug("Input: {0}, Expected: {1}", x, y)
        Seq.tryFindIndex (fun (f: ST) -> f.apply(x) = y) fs ?|> (fun k -> k + 1)

    let testMany p = List.map (testOne p)

    let synthesizeAndTest (config: SolverConfig) (examples: Example list, tests: Example list)
                          : int option list option =
        synthesize config (transform examples) ?|> (fun p -> testMany p (transform tests))

    run config file idFilter numTrainingExamples synthesizeAndTest

let runBenchmarksNaiveT (config: SolverConfig) (file: string) 
                        (idFilter: int -> bool) (numTrainingExamples: int) =
    let transform (examples: Example list): TExample list =
        examples ||> (fun (eq, err, fix) -> (eq :: tokenizeErr err, fix))

    let synthesize (config: SolverConfig) (examples: TExample list): STVSA option =
        let solver = Solver config in solver.Solve examples

    let testOne (fs: STVSA) (test: TExample) : int option =
        let x, y = test
        Log.Debug("Input: {0}, Expected: {1}", x, y)
        Seq.tryFindIndex (fun (f: ST) -> f.apply(x) = y) fs ?|> (fun k -> k + 1)

    let testMany p = List.map (testOne p)

    let synthesizeAndTest (config: SolverConfig) (examples: Example list, tests: Example list)
                          : int option list option =
        synthesize config (transform examples) ?|> (fun p -> testMany p (transform tests))

    run config file idFilter numTrainingExamples synthesizeAndTest

let onlineTest (file: string) testRunner =
    let data = parseBenchmark file noFilter
    let total = data.Length
    let results =
        [for (i, es) in data ->
            Log.Info("Running benchmark #{0} of {1}", i, total)
            let rs = es ||> testRunner
            in (i, rs)]
    Log.Info("Results:")
    [for (i, rs) in results ->
        (i, List.map (fun x -> match x with Some k -> k | _ -> -1) rs)
    ] |> List.iter (fun (i, rs) -> printfn "#%d: %A" i rs)

let demo1 () =
    let eq1 = "+ alpha"
    let err1 = "alpha is a letter, + shall be -"
    let fix1 = "- \\alpha"
    let example1 = (eq1, err1, fix1)

    let eq2 = "- beta"
    let err2 = "beta is a letter, - shall be +"
    let fix2 = "+ \\beta"
    let example2 = (eq2, err2, fix2)
    let examples = [example1; example2]

    let config = { maxK = 1; solver = SolverName.STLang }

    match synthesizeRule config examples with
    | Some rule -> printfn "%A" rule
    | None -> printfn "Fail to synthesize rule."

    synthesizeRulesAndTest config ([example1], [example2]) |> printfn "%A"

let demo2 () =
    let eq1 = "alpha + beta + gamma"
    let err1 = "alpha , beta and gamma are letters"
    let fix1 = "\\alpha - \\beta - \\gamma"
    let example1 = (eq1, err1, fix1)

    let eq2 = "gamma * zeta * varphi"
    let err2 = "gamma , zeta and varphi are letters"
    let fix2 = "\\gamma - \\zeta - \\varphi"
    let example2 = (eq2, err2, fix2)
    let examples = [example1; example2]

    let config = { maxK = 1; solver = SolverName.STLang }

    match synthesizeRule config examples with
    | Some rule -> printfn "%A" rule
    | None -> printfn "Fail to synthesize rule."

    synthesizeRulesAndTest config ([example1], [example2]) |> printfn "%A"

// options
type SolverOptions =
    | [<MainCommand; Mandatory>] Benchmarks of file: string
    | Ids of ids: int list
    | Skips of skips: int list
    | [<AltCommandLine("-s")>] Solver of SolverName
    | [<AltCommandLine("-k")>] Max_K of k: int
    | [<AltCommandLine("-n")>] Num_Training_Example of n: int
with interface IArgParserTemplate with
        member s.Usage = match s with
                         | Benchmarks _ -> "benchmarks file"
                         | Ids _ -> "specify a list of benchmarks (by ids) expected to run"
                         | Skips _ -> "skip a list of benchmarks (by ids), run the others"
                         | Solver _ -> "specify the string transformer synthesizer, default stlang"
                         | Max_K _ -> "synthesize top k programs, default 1"
                         | Num_Training_Example _ -> 
                           "for each benchmark, take the first k examples for learning, default 1"
and OnlineLearnOptions =
    | [<AltCommandLine("-L")>] Load of file: string
    | [<AltCommandLine("-d")>] Dump of file: string
    | [<AltCommandLine("-t")>] Test of file: string
    | [<CliPrefix(CliPrefix.None)>] Learn of ParseResults<SolverOptions>
    | [<CliPrefix(CliPrefix.None)>] Interactive of ParseResults<SolverOptions>
with interface IArgParserTemplate with
        member s.Usage = match s with
                         | Learn _ -> "learning a rulebase"
                         | Test _ -> "test benchmarks"
                         | Interactive _ -> "test benchmarks (interactive mode)"
                         | Dump _ -> "dump the rulebase into a file"
                         | Load _ -> "load rulebase from a file"
and CLIOptions =
    | [<AltCommandLine("-log")>] Log_Level of Logger.LogLevel
    | [<AltCommandLine("-p")>] Plain_Log
    | [<CliPrefix(CliPrefix.None)>] Demo
    | [<CliPrefix(CliPrefix.None)>] Eval of ParseResults<SolverOptions>
    | [<CliPrefix(CliPrefix.None)>] Naive of ParseResults<SolverOptions>
    | [<CliPrefix(CliPrefix.None)>] NaiveT of ParseResults<SolverOptions>
    | [<CliPrefix(CliPrefix.None)>] Online of ParseResults<OnlineLearnOptions>
with interface IArgParserTemplate with
        member s.Usage = match s with
                         | Log_Level _ -> "specify the log level, default info."
                         | Plain_Log _ -> "enable plain log mode (default colorful log)."
                         | Demo -> "running demos."
                         | Eval _ -> "evaluation mode."
                         | Naive _ -> "naive evaluation mode."
                         | NaiveT _ -> "naive evaluation mode with tokenizing err message."
                         | Online _ -> "evaluation in online mode."

let runBenchmarks (r: ParseResults<SolverOptions>) runner =
        let config = { maxK = r.GetResult (Max_K, defaultValue = 1);
                       solver = r.GetResult (Solver, defaultValue = SolverName.STLang) }
        Log.Debug("Solver config: {0}", config)
        let benchmarks = r.GetResult Benchmarks
        let numTrainingExamples = r.GetResult (Num_Training_Example, defaultValue = 1)
        let idFilter = match r.TryGetResult Ids with
                       | Some ids -> (fun i -> List.contains i ids)
                       | None -> match r.TryGetResult Skips with
                                 | Some ids -> (fun i -> not (List.contains i ids))
                                 | None -> noFilter

        runner config benchmarks idFilter numTrainingExamples

let onlineLearnBenchmarks (r: ParseResults<SolverOptions>) learner tester =
    let config = { maxK = r.GetResult (Max_K, defaultValue = 1);
                    solver = r.GetResult (Solver, defaultValue = SolverName.STLang) }
    Log.Debug("Solver config: {0}", config)
    let file = r.GetResult Benchmarks
    let numTrainingExamples = r.GetResult (Num_Training_Example, defaultValue = 1)
    let idFilter = match r.TryGetResult Ids with
                    | Some ids -> (fun i -> List.contains i ids)
                    | None -> noFilter
    let data = parseBenchmark file idFilter
    let total = data.Length
    [for (i, es) in data -> es |> (List.splitAt numTrainingExamples >> fst >> learner config)] |> ignore

    // test using the last example of each group
    [for (i, es) in data ->
        Log.Info("Running benchmark #{0} of {1}", i, total)
        match List.last es |> tester with
        | Some (k: int) -> Log.Info("Success: {0} rule(s) attempted", k)
        | None -> Log.Info("Failure: no rule applicable")
    ] |> ignore

[<EntryPoint>]
let main argv =
    let exe = "EqFix.exe"
    let errorHandler = ProcessExiter()
    let parser = ArgumentParser.Create<CLIOptions>(programName = exe, errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv

    // setup loggers
    Log.ShowColor <- not (results.Contains Plain_Log)
    Log.DisplayLevel <- results.GetResult (Log_Level, defaultValue = Logger.LogLevel.INFO)

    // modes
    let (|ModeDemo|ModeEval|ModeNaive|ModeNaiveT|ModeOL|ModeNone|) (r: ParseResults<CLIOptions>) = 
        if r.Contains Demo then ModeDemo
        else if r.Contains Eval then ModeEval
        else if r.Contains Naive then ModeNaive
        else if r.Contains NaiveT then ModeNaiveT
        else if r.Contains Online then ModeOL
        else ModeNone

    // do different things based on different mode
    match results with
    | ModeDemo ->
        demo1 ()
        demo2 ()
    | ModeEval ->
        runBenchmarks (results.GetResult Eval) runBenchmarksEqFix
    | ModeNaive ->
        runBenchmarks (results.GetResult Naive) runBenchmarksNaive
    | ModeNaiveT ->
        runBenchmarks (results.GetResult NaiveT) runBenchmarksNaiveT
    | ModeOL ->
        let ruleLib = RuleLib ()
        let handler = function
                      | Load file -> ruleLib.Load file
                      | Learn r -> onlineLearnBenchmarks r ruleLib.Learn ruleLib.Test
                      | Test file -> onlineTest file ruleLib.Test
                      | Interactive r ->
                            let config = { maxK = r.GetResult (Max_K, defaultValue = 1);
                                            solver = r.GetResult (Solver, defaultValue = SolverName.STLang) }
                            Log.Debug("Solver config: {0}", config)
                            let file = r.GetResult Benchmarks
                            onlineTest file (ruleLib.TestInteractive config)
                      | Dump file -> ruleLib.Dump file

        (results.GetResult Online).GetAllResults()
        |> List.sortBy (function 
                        | Load _ -> 1
                        | Learn _ -> 2
                        | Test _ -> 3
                        | Interactive _ -> 4
                        | Dump _ -> 5)
        |> List.iter handler
    | ModeNone ->
        printfn "ERROR: missing running mode (demo|eval|naive)."
        parser.PrintUsage() |> printfn "%s"
    
    0 // return an integer exit code
