module EqFix.Bench

open EqFix.Util
open EqFix.Synthesizer
open EqFix.Transformer

open Argu

let rawBench (config: SolverConfig) (file: string) (idFilter: int -> bool) (numTrainingExamples: int) runner =
    Log.Info("Loading benchmark: {0}", file)
    Log.Info("Number of training examples: {0}", numTrainingExamples)

    let data = REPL.loadExampleGroups file idFilter
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

let flashFillRunner (config: SolverConfig) (examples: Example list, tests: Example list) =
    // Note: Flashfill does not support error message, so `eq` and `err` are regarded as
    // two input columns in spreadsheet
    let transform (examples: Example list): TExample list =
        examples ||> (fun (eq, err, fix) -> ([eq; err], fix))

    let synthesize (examples: TExample list): STVSA option =
        let solver = Solver config in solver.Solve examples

    let testOne (fs: STVSA) (test: TExample) : int option =
        let x, y = test
        Log.Debug("Input: {0}, Expected: {1}", x, y)
        Seq.tryFindIndex (fun (f: ST) -> f.apply(x) = y) fs ?|> (fun k -> k + 1)

    let testMany p = List.map (testOne p)

    synthesize (transform examples) ?|> (fun p -> testMany p (transform tests))

// TrainOptions + '--flashfill'
type BenchOptions =
    | [<MainCommand; Mandatory>] BenchFile of file: string
    | Only of ids: int list
    | [<AltCommandLine("-n")>] Num_Training_Example of n: int
    | FlashFill
    | [<AltCommandLine("-k")>] Max_K of k: int
    | [<AltCommandLine("-s")>] Solver of SolverName
with interface IArgParserTemplate with
        member s.Usage =
            match s with
            | BenchFile _ -> "data file"
            | Only _ -> "specify a list of example groups (by ids) expected to run"
            | Num_Training_Example _ -> "for each group, take the first k examples for learning, default 1"
            | FlashFill -> "use FlashFill for the entire synthesis"
            | Max_K _ -> "synthesize top k programs, default 1"
            | Solver _ -> "specify the string transformer synthesizer, default stlang"

let bench (r: ParseResults<BenchOptions>) =
    let config = { maxK = r.GetResult (Max_K, defaultValue = 1);
                   solver = if r.TryGetResult FlashFill |> Option.isSome then SolverName.PROSE
                            else r.GetResult (Solver, defaultValue = SolverName.STLang) }
    Log.Debug("Solver config: {0}", config)
    let benchmarks = r.GetResult BenchFile
    let numTrainingExamples = r.GetResult (Num_Training_Example, defaultValue = 1)
    let idFilter = match r.TryGetResult Only with
                    | Some ids -> (fun i -> List.contains i ids)
                    | None -> REPL.noFilter
    let runner = if r.TryGetResult FlashFill |> Option.isSome then flashFillRunner else synthesizeRulesAndTest

    rawBench config benchmarks idFilter numTrainingExamples runner
