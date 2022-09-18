module EqFix.RuleLib

open EqFix.Extractor
open EqFix.Transformer
open EqFix.Synthesizer
open EqFix.Util

open System.Collections.Generic
open System.Linq
open System.IO
open MBrace.FsPickler

type RuleLib () =
    let xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)

    let rules = new List<RuleVSA>()

    let applyPattern1 (errPattern: ErrPattern) (relaxers: Relaxer list)
                    (input: Input) : (Pattern * PEnv) option =
        let merge (env: Env) (pEnv: PEnv) : PEnv =
            let rec add (pairs: (VarId * string) list) (pEnv: PEnv) : PEnv =
                match pairs with
                | [] -> pEnv
                | (v, s) :: ps -> add ps (insert (EnvVar v) s pEnv)

            add (Map.toList env) pEnv
        
        let eq, err = tokenizeInput input
        if errPattern.Length <> err.Length then None
        else
            match matchErr errPattern err with
            | Some env ->
                let p = genPattern eq env
                match relaxPattern relaxers p with
                | Some p' -> matchPattern p' eq ?|> (fun env' -> p', merge env env')
                | None -> None
            | None -> None

    let tryTest1 (rules: RuleVSA) (test: Example) : int option =
        let errPattern, relaxers, stVSA = rules
        let eq, err, fix = test
        let input = (eq, err)

        match applyPattern1 errPattern relaxers input with
        | Some (p, eqEnv) ->
            match matchPattern p fix with // `fix` should also match `p`
            | Some fixEnv ->
                let n = stVSA.Count
                stVSA
                |>  mapKeysAndValues (fun ((v, ex), fs) ->
                        v, // PVar of Schema, used as ID of transformer
                        let vs = v :: List.map EnvVar ex in // input vars
                        match tryFindMany vs eqEnv with
                        | Some x -> // input vars
                            match Map.tryFind v fixEnv with
                            | Some y -> // output
                                Log.Debug("ST Test on {0}: Input: {1}, Expected: {2}", v, x, y);
                                let tmp = List.tryFindIndex (fun (f: ST) -> f.apply(x) = y) fs ?|> (fun k -> k + 1) 
                                // find first `f` s.t. output = expected, return its index
                                // or `None` if search bound is exceeded
                                match tmp with
                                | Some k -> Log.Success("Succeed with {0} tries", k)
                                | None -> Log.Info("Waste {0} tries", Seq.length fs)
                                tmp
                            | None -> None
                        | None -> None
                    )
                |>  values
            ||>? id
                ?|> (fun ks -> 1 - n + List.sum ks)
            | None -> None
        | None -> None
    
    member this.Empty () = not (rules.Any())

    member this.Dump (file: string) =
        // TODO: Implement Serializer for STProgram
        if this.Empty () then Log.Warning("RuleLib: no rules found, dump ignore.")
        else
            let bytes = xmlSerializer.Pickle rules
            File.WriteAllBytes(file, bytes)
            Log.Success("RuleLib: {0} rules dumped to file {1}.", rules.Count, file)

    member this.Load (file: string) =
        // TODO: Implement Serializer for STProgram
        let bytes = File.ReadAllBytes(file)
        let rules = xmlSerializer.UnPickle<List<RuleVSA>> bytes
        rules.AddRange(rules)
        Log.Success("RuleLib: {0} rules loaded from file {1}, totally {2}.", 
            rules.Count, file, rules.Count)

    member this.Learn (config: SolverConfig) (examples: Example list) = 
        match synthesizeRules config examples with
        | Some r -> 
            rules.Add(r)
            Log.Success("RuleLib: rule synthesized, totally {0}.", rules.Count)
        | None -> Log.Failure("RuleLib: fail to synthesize rule.")

    member this.Test (test: Example): int option = findAndReturn (fun r -> tryTest1 r test) rules

    member this.TestInteractive (config: SolverConfig) (test: Example): int option =
        match this.Test test with
        | Some k -> Some k
        | None -> // test failure
            Log.Warning("Test failure")
            Log.Warning("============")
            let (eq, err, fix) = test
            Log.Warning("Equation: {0}", eq)
            Log.Warning("Error: {0}", err)
            Log.Warning("Synthesis using fix: {0}", fix)
            this.Learn config [test]
            None

    member this.Apply (input: Input): string seq option =
        Seq.fold (fun s -> fun r -> match s with 
                                    | Some k -> Some k
                                    | None -> applyRules1 r input) None rules
