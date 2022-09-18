module EqFix.Synthesizer

open EqFix.Util
open EqFix.Extractor
open EqFix.Transformer

// Input: an equation with an error message.
type Input = string * string

// Input-output example: input (an equation with an error message) with a fixed equation.
type Example = string * string * string

// Split an error message (one string) into many parts (a list of string).
let tokenizeErr (err: string) : string list =
    let startChars = [|'\''; '`'|]
    let endChars = [|','; '.'; '\''; '`'|]
    let endOfLine = [|'.'; '?'|]
    let seps = [|' '; '/'|]

    err.TrimEnd(endOfLine).Split(seps)
    |> Array.toList
    ||> (fun e -> if e.Length = 1 then e else e.TrimStart(startChars).TrimEnd(endChars))

let tokenizeInput = fun (eq, err) -> (eq, tokenizeErr err)

let tokenizeExamples = List.map (fun (eq, err, fix) -> (eq, tokenizeErr err, fix))

// Rule: an err pattern, relaxers with string transformers.
type Rule = ErrPattern * Relaxer list * Map<Schema, ST>

// Rule VSA: a rule in which string transformers are presented as VSAs.
type RuleVSA = ErrPattern * Relaxer list * Map<Schema, STVSA>

// First pass of synthesizing rules: extracting `TExamples`.
let extractTExamples (examples: Example list) : (ErrPattern * Relaxer list *
                                                 Map<Schema, TExample list>) option =
    let sExamples = tokenizeExamples examples
    let errPattern = synthesizeErrPattern sExamples
    Log.Debug("Err Pattern synthesized: {0}", showList errPattern)
    match synthesizeRelaxers errPattern sExamples with
    | Some relaxers ->
        Log.Debug("Relaxers synthesized: {0}", showList relaxers)
        genTExamples errPattern relaxers sExamples ?|> (fun t -> (errPattern, relaxers, t))
    | None -> Log.Failure("Failed to synthesize relaxers."); None

// Synthesize a rule: select the top-ranked string transformers.
let synthesizeRule (config: SolverConfig) (examples: Example list) : Rule option =
    let solver = Solver config
    let closure () =
        match extractTExamples examples with
        | Some (errPattern, relaxers, es) ->
            mapValues (solver.SolveTop) es
            |> tryMapValues id
            |> Option.map (fun t -> (errPattern, relaxers, t))
        | None -> Log.Failure("Rule synthesis failed."); None
    
    measureRuntime closure "Rule synthesis"

// Synthesize rule VSAs: including all candidate string transformers.
let synthesizeRules (config: SolverConfig) (examples: Example list) : RuleVSA option =
    let solver = Solver config
    let closure () =
        match extractTExamples examples with
        | Some (errPattern, relaxers, es) ->
            mapValues (solver.Solve) es
            |> tryMapValues id
            |> Option.map (fun t -> (errPattern, relaxers, t))
        | None -> Log.Failure("Rule synthesis failed."); None
    
    measureRuntime closure "Rule synthesis"

// First pass of evaluating rules: extracting values of parameters used in string tranformation.
// NOTE these vars should contain both complex ones (extracted from relaxed pattern) 
// and also normal ones (extracted from `err`).
let applyPattern (errPattern: ErrPattern) (relaxers: Relaxer list)
                 (input: Input) : (Pattern * PEnv) option =
    let merge (env: Env) (pEnv: PEnv) : PEnv =
        let rec add (pairs: (VarId * string) list) (pEnv: PEnv) : PEnv =
            match pairs with
            | [] -> pEnv
            | (v, s) :: ps -> add ps (insert (EnvVar v) s pEnv)

        add (Map.toList env) pEnv
    
    let eq, err = tokenizeInput input
    match matchErr errPattern err with
    | Some env ->
        let p = genPattern eq env
        match relaxPattern relaxers p with
        | Some p' -> matchPattern p' eq ?|> (fun env' -> p', merge env env')
        | None -> None
    | None -> Log.Error("Cannot match {0} with {1}", showList errPattern, showList err); None

// Apply a `rule` on user-provided `inputs`.
let applyRule (rule: Rule) (inputs: Input list) : string option list =
    let errPattern, relaxers, st = rule

    let on (input: Input) : string option =
        match applyPattern errPattern relaxers input with
        | Some (p, env) ->
            st |> mapKeysAndValues (fun ((v, ex), f) ->
                    v, // PVar of Schema, used as ID of transformer
                    let vs = v :: List.map EnvVar ex in // input vars
                    let x = findMany vs env in // input vals
                    f.apply(x))
               |> applyPEnv p
               |> Some
        | None -> None

    List.map on inputs

// Apply a `ruleVSA` (i.e. many rules) on one user-provided `input`.
let applyRules1 (ruleVSA: RuleVSA) (input: Input) : string seq option =
    let errPattern, relaxers, stVSA = ruleVSA
    match applyPattern errPattern relaxers input with
        | Some (p, env) ->
            stVSA |> mapKeysAndValues (fun ((v, ex), fs) ->
                        v, // PVar of Schema, used as ID of transformer
                        let vs = v :: List.map EnvVar ex in // input vars
                        let x = findMany vs env in // input vals
                        List.toSeq (List.map (fun (f: ST) -> f.apply(x)) fs))
                  |> uncollectMap
                  |> Seq.map (applyPEnv p)
                  |> Some
        | None -> None

// Apply a `ruleVSA` (i.e. many rules) on user-provided `inputs`.
let applyRules (ruleVSA: RuleVSA) (inputs: Input list) : string seq option list =
    List.map (applyRules1 ruleVSA) inputs

// Test a `rule` on many `tests`.
let testRule (rule: Rule) (tests: Example list) : bool option list =
    let inputs, expects = tests |> List.map (fun (eq, err, fix) -> (eq, err), fix)
                                |> List.unzip
    let outputs = applyRule rule inputs
    List.zip outputs expects |> List.map (fun (o, e) -> Option.map (fun s -> s = e) o)

// Synthesize a (top-ranked) rule by `examples`, and then test it on `tests`.
let synthesizeRuleAndTest (config: SolverConfig) 
                          (examples: Example list, tests: Example list) : bool option list option =
    synthesizeRule config examples ?|> (fun r -> testRule r tests)

// Find the top-ranked `rule` from `rules` (i.e. RuleVSA) s.t. the `test` passes.
// Return values:
// - `None`: pattern match failure or not found
// - `Some (Some k)`: success and `k` denotes the total number of tries until the test passes
// Definition for the _total number of tries_:
// Since there are possibly many string transformers like `t1, t2, ..., tn`,
// instead of first apply all `t1, t2, ..., tn` and get a final result string (`fix`),
// and then compare it to the expected output,
// we first extract the expected output for each transformer `ti` based on the entire output,
// as we do in `genTExamples` (by matching the same pattern on `eq` and `fix`),
// and now we can compare the result of `ti` with the expected output of `ti`.
// If `t1` requires `k1` tries, ..., `tn` requires `kn` tries, `k1, ..., kn <= maxK`,
// the _total number of tries_ is given by `(k1 - 1) + ... + (kn - 1) + 1`,
// to ensure that if every transformer only need to try the top-ranked program,
// then the total number of tries is 1.
let tryTest (rules: RuleVSA) (test: Example) : int option =
    let errPattern, relaxers, stVSA = rules
    let eq, err, fix = test
    let input = (eq, err)

    match applyPattern errPattern relaxers input with
    | Some (p, eqEnv) ->
        match matchPattern p fix with // `fix` should also match `p`
        | Some fixEnv ->
            Log.Debug("p={0};;fixEnv={1} gives you {2}", p,fixEnv,fix)
            let n = stVSA.Count
            stVSA
            |>  mapKeysAndValues (fun ((v, ex), fs) ->
                    v, // PVar of Schema, used as ID of transformer
                    let vs = v :: List.map EnvVar ex in // input vars
                    let x = findMany vs eqEnv in // input vals
                    let y = find v fixEnv in // expected output
                    Log.Debug("ST Test on {0}: Input: {1}, Expected: {2}", v, x, y);
                    List.tryFindIndex (fun (f: ST) -> f.apply(x) = y) fs ?|> (fun k -> k + 1))
                    // find first `f` s.t. output = expected, return its index
                    // or `None` if search bound is exceeded
            |>  values
           ||>? id
            ?|> (fun ks -> 1 - n + List.sum ks)
        | None -> Log.Error("Fix \"{0}\" cannot match \"{1}\"", fix, p); None
    | None -> Log.Failure("Failed to match input \"{0}\", \"{1}\"", eq, err); None

// Synthesize many rules by `examples`, and then test it on `tests`.
// Returns the _total number of tries_ for each test.
let synthesizeRulesAndTest (config: SolverConfig) (examples: Example list, tests: Example list)
                           : int option list option =
    let test (rules: RuleVSA) (tests: Example list) : int option list =
        List.map (tryTest rules) tests
    
    synthesizeRules config examples ?|> (fun r -> test r tests)