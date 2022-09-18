module EqFix.Extractor

open EqFix.Util

// Simple variables, extracted from the err message.
// ASSUME >= 0
type VarId = int

// Each matcher matches a token of the err message.
// `Str(s)` matches with a const string `s`.
// `Var(v)` matches with anything and store the matched value of `v` in the environment.
// `Any` matches with anything and throws the matched value away.
[<StructuredFormatDisplay("{AsString}")>]
type Matcher = Str of string
             | Var of VarId
             | Any
with override this.ToString() =
             match this with
             | Str(s) -> sprintf "%A" s
             | Var(v) -> sprintf "Var(%i)" v
             | Any -> "_"
     member this.AsString = this.ToString()

// An err pattern which contains a list of matchers, each match a token of the err message.
type ErrPattern = Matcher list

// Pattern variables.
// A `EnvVar` is just a simple variable.
// NOTE `EnvVar(-1)` is a special variable referring to the entire input string itself.
// Other PVars are fresh variables introduced by relaxers, named relaxed variables.
[<StructuredFormatDisplay("{AsString}")>]
type PVar = EnvVar of VarId
          | LRelaxVar of PVar
          | RRelaxVar of PVar
          | BRelaxVar of PVar * PVar
with override this.ToString() =
          match this with
          | EnvVar(-1) -> "self"
          | EnvVar(v) -> sprintf "Var(%i)" v
          | LRelaxVar(v) -> sprintf "LVar(%A)" v
          | RRelaxVar(v) -> sprintf "RVar(%A)" v
          | BRelaxVar(v1, v2) -> sprintf "BVar(%A, %A)" v1 v2
      member this.AsString = this.ToString()

// This is simply a mutual recursive definition of VPattern and SPattern.
// An equivalent definition may be `type VPattern = ... and SPattern = ...`,
// but `StructuredFormatDisplay` property may not support this!
[<StructuredFormatDisplay("{AsString}")>]
type 'SP RawVPattern = VCons of PVar * 'SP // VCons of PVar * SPattern
                     | VNil
with override this.ToString() =
                     match this with
                     | VCons(v, sp) -> sprintf "%A :: %A" v sp
                     | VNil -> "Nil"
     member this.AsString = this.ToString()

[<StructuredFormatDisplay("{AsString}")>]
type SPattern = SCons of string * SPattern RawVPattern // SCons of string * VPattern
              | SNil
with override this.ToString() = 
              match this with
              | SCons(s, vp) -> sprintf "%A :: %A" s vp
              | SNil -> "Nil"
     member this.AsString = this.ToString()

type VPattern = SPattern RawVPattern

// A pattern either starts with a var (a VPattern) or a const string (a SPattern).
// Vars and strings must be occurred interchangeably in a pattern.
type Pattern = Either<VPattern, SPattern>

// A special hole pattern in case no interesting variables are extracted from err message.
// NOTE `EnvVar(-1)` is a special variable referring to the entire input string itself,
// which should be inserted to `PEnv`.
let HolePattern : Pattern = Left (VCons(EnvVar(-1), SNil))

// Relaxers.
// Each relaxer substitutes some sub pattern with a pattern variable.
// `NoRelax(v): .. EnvVar(v) .. => .. EnvVar(v) ..`
// `LRelax(p => v): ^ s p .. => ^ LRelaxVar(v) ..`
// `RRelax(p => v): .. p s $ => .. RRelaxVar(v) $`
// `BRelax(p1 => v1, p2 => v2): .. p1 s p2 .. => .. BRelaxVar(v1, v2) ..`
[<StructuredFormatDisplay("{AsString}")>]
type Relaxer = NoRelax of VarId
             | LRelax of Relaxer
             | RRelax of Relaxer
             | BRelax of Relaxer * Relaxer
with override this.ToString() =
             match this with
             | NoRelax(-1) -> sprintf "RelaxAll"
             | NoRelax(v) -> sprintf "Var(%i)" v
             | LRelax(v) -> sprintf "LRelax(%A)" v
             | RRelax(v) -> sprintf "RRelax(%A)" v
             | BRelax(v1, v2) -> sprintf "BRelax(%A, %A)" v1 v2
      member this.AsString = this.ToString()

// Special case for hole pattern: `NoRelax(-1) : s => EnvVar(-1)`
let HoleRelaxer : Relaxer = NoRelax(-1)

// An environment which stores the value (a string) for each variables.
type Env = Map<VarId, string>

// An environment which stores the value (a string) for each pattern variables.
type PEnv = Map<PVar, string>

let emptyEnv: Env = Map.empty

// Semantics of matchers.
let matchMatcher (matcher: Matcher, str: string) (env: Env) : Env option =
    match matcher with
    | Str(s) -> if str = s then Some env else None
    | Var(i) -> Some (insert i str env)
    | Any -> Some env

// Semantics of an err pattern, which indeed extracts some interesting variables.
let matchErr (pattern: ErrPattern) (err: string list) : Env option =
    let f e p = Option.bind (matchMatcher p) e
    if pattern.Length <> err.Length
    then Log.Error("Err message {0} and err pattern {1} have different lengths.", 
                   showList err, showList pattern); None
    else List.zip pattern err |> List.fold f (Some emptyEnv)

// Given an `env` which contains the interesting variables extracted by `matchErr`,
// we can find a pattern that represents the concrete string `str`.
// This function generates the most strict pattern for `str`,
// say we only substitute the parts that are seen in `env` as a variable.
// If some value in `env` can not be matched, then it is ignored.
let genPattern (str: string) (env: Env) : Pattern =
    let n = str.Length

    let rec genV (offset: int) (vars: (VarId * PosRange) list): VPattern =
        match vars with
        | [] -> VNil
        | (v, (l, r)) :: vs ->
            assert(offset = l)
            VCons(EnvVar(v), genS (r + 1) vs)
    and genS (offset: int) (vars: (VarId * PosRange) list): SPattern =
        if offset >= n then SNil
        else let next = match vars with
                        | [] -> n
                        | (_, (l, _)) :: _ -> l
             in SCons(str.[offset..next - 1], genV next vars)

    let gen (vars: (VarId * PosRange) list): Pattern =
        match vars with
        | [] -> Right(genS 0 [])
        | (_, (l, _)) :: _ -> if l = 0 then Left(genV 0 vars) else Right(genS 0 vars)

    env
    |> mapValues (searchString str)
    |> Map.toList
    |> List.collect (fun (l, xs) -> List.map (fun x -> (l, x)) xs)
    |> List.sortBy (snd >> fst) // by the lower bound of the position range
    |> gen

// Given a new string `str`, relax the `pattern` only if necessary s.t. 
// `str` matches the new pattern.
// The relax process is lazy, keeping the pattern as strict as possible.
let genRelaxedPattern (pattern: Pattern) (str: string) : Pattern =
    let rec onS (pattern: SPattern) (prev: PVar) (offset: int) : VPattern option =
        // prev is not yet added to the output pattern
        match pattern with
        | SNil -> None
        | SCons(s, vp) ->
            match str.IndexOf(s, offset) with
            | -1 -> // relax
                match vp with
                | VNil -> 
                    Some (VCons(RRelaxVar(prev), SNil)) // prev s => RRelaxVar(prev)
                | VCons(next, sp) ->
                    Some (VCons(BRelaxVar(prev, next), sp)) // prev s next => BRelaxVar(prev, next)
            | k ->
                match vp with
                | VNil -> // NOTE `str` may have extra characters, when relaxing is still needed
                    if (k + s.Length) = str.Length then None // no extra, safe
                    else Some (VCons(RRelaxVar(prev), SNil)) // relax: prev s => RRelaxVar(prev)
                | VCons(v, sp) ->
                    onS sp v (k + s.Length) |> Option.map (fun vp' -> VCons(prev, SCons(s, vp')))

    let relax (pattern: Pattern): Pattern option =
        match pattern with
        | Left VNil -> None
        | Left (VCons(v, sp)) -> onS sp v 0 |> Option.map Left
        | Right SNil -> None
        | Right (SCons(_, VNil)) ->
            Log.Debug("Pattern is a const string: {0}.", pattern)
            Some HolePattern // the whole pattern is a string: gen hole pattern
        | Right (SCons(s, VCons(next, sp))) -> // NOTE if match, `str` must start with `s`
            if str.StartsWith(s)
            then onS sp next s.Length |> Option.map (fun vp -> Right (SCons(s, vp)))
            else Some (Left (VCons(LRelaxVar(next), sp))) // relax: s next => LRelaxVar(next)

    let rec relaxUntilFixPoint (pattern: Pattern): Pattern =
        match relax pattern with
        | None -> pattern // fix point is reached
        | Some p -> relaxUntilFixPoint p

    relaxUntilFixPoint pattern

// Given a genenrated relaxed `pattern`, extract the relaxers.
let extractRelaxers (pattern: Pattern) : Relaxer list =
    let rec visitVar (var: PVar) : Relaxer =
        match var with
        | EnvVar(v) -> NoRelax(v)
        | LRelaxVar(v) -> LRelax(visitVar v)
        | RRelaxVar(v) -> RRelax(visitVar v)
        | BRelaxVar(v1, v2) -> BRelax(visitVar v1, visitVar v2)

    let rec onV (pattern: VPattern) : Relaxer list =
        match pattern with
        | VNil -> []
        | VCons(EnvVar(_), sp) -> onS sp // skip simple variables
        | VCons(v, sp) -> (visitVar v) :: onS sp // visit relaxed variables
    and onS (pattern: SPattern) : Relaxer list =
        match pattern with
        | SNil -> []
        | SCons(_, vp) -> onV vp

    match pattern with
    | p when p = HolePattern -> [HoleRelaxer] // special case for hole pattern
    | Left vp -> onV vp
    | Right sp -> onS sp

// Apply a `relaxer` to a `pattern`.
let evalRelaxer (relaxer: Relaxer) (pattern: Pattern) : Pattern option =
    // ASSUME the head of `pattern` is the first `EnvVar(v)` that `relaxer` needs, called pivot.
    // NOTE `LRelax` is not allowed when `allowLeftRelax` is true.
    // NOTE When `allowLeftRelax` is true, the prev const string has not yet been added to 
    //      the output pattern and it may be consumed by the `LRelax`.
    //      Therefore, we have to return a bool indicating whether it has been comsumed.
    // ENSURE the output vpattern must be non-Nil.
    let rec onV (relaxer: Relaxer) (pattern: VPattern) 
                (allowLeftRelax: bool) : (VPattern * bool) option =
        match relaxer with
        | NoRelax(v0) ->
            match pattern with
            | VCons(EnvVar(v), _) when v = v0 ->
                // .. EnvVar(v) :: _ => .. EnvVar(v) :: _
                Some (pattern, false)
            | _ -> None
        | LRelax(r) ->
            match onV r pattern false with
            | Some (VCons(v, sp), false) when allowLeftRelax ->
                // _ :: v :: sp, where `v` is the result after eval `r`
                // => RRelaxVar(v) :: sp
                Some (VCons(LRelaxVar(v), sp), true) // here we consume the prev string
            | _ -> None
        | RRelax(r) ->
            match onV r pattern allowLeftRelax with
            | Some (VCons(v, SCons(_, VNil)), consumed) ->
                // .. v :: _ :: Nil, where `v` is the result after eval `r`
                // => .. RRelaxVar(v) :: Nil
                Some (VCons(RRelaxVar(v), SNil), consumed)
            | _ -> None
        | BRelax(r1, r2) ->
            match onV r1 pattern allowLeftRelax with
            | Some (VCons(v1, SCons(_, vp)), consumed) ->
                // v1 :: _ :: vp, where `v1` is the result after eval `r1`
                match onV r2 vp false with
                | Some (VCons(v2, sp), false) ->
                    // v1 :: _ :: v2 :: sp, where `v2` is the result after eval `r2`
                    // => BRelaxVar(v1, v2) :: sp
                    Some (VCons(BRelaxVar(v1, v2), sp), consumed)
                | _ -> None
            | _ -> None
    
    let rec findPivot (relaxer: Relaxer) : VarId =
        match relaxer with
        | NoRelax(v) -> v
        | LRelax(r) -> findPivot r
        | RRelax(r) -> findPivot r
        | BRelax(r1, _) -> findPivot r1 // left first
    let pivot = findPivot relaxer

    // ASSUME when `prev` is set, `allowLeftRelax` is true.
    let rec eval (pattern: VPattern) (prev: string option): Pattern option =
        match pattern with
        | VCons(EnvVar(v), sp) when v = pivot ->
            let tryResult = 
                match prev with
                | Some s -> // `LRelax` is allowed. If `consumed`, we do not need to add `prev`.
                    onV relaxer pattern true ?|> (fun (vp', consumed) ->
                        if consumed then Left (vp') else Right (SCons(prev.Value, vp')))
                | None -> // `LRelax` is not allowed, `prev` has already been added.
                    onV relaxer pattern false ?|> (fun (vp', consumed) ->
                        assert(not consumed); Left (vp'))
            match tryResult with
            | Some r -> Some r
            | None -> // skip this and continue trying to find
                match sp with
                | SNil -> None // already last, fail
                | SCons(s, vp) ->
                    match eval vp None with
                    | Some (Left vp') ->
                        let tmp = VCons(EnvVar(v), SCons(s, vp'))
                        if prev.IsSome then Some (Right (SCons(prev.Value, tmp)))
                        else Some (Left tmp)
                    | _ -> None // NOTE sp is not possible
        | VCons(v, SCons(s, vp)) -> // `LRelax` is not allowed
            match eval vp None with
            | Some (Left vp') -> Some (Left (VCons(v, SCons(s, vp'))))
            | _ -> None // NOTE sp is not possible
        | _ -> None
    
    if relaxer = HoleRelaxer then Some HolePattern // special case for hole relaxer
    else match pattern with
         | Left vp -> eval vp None // no leading string, thus `LRelax` is not allowed
         | Right SNil -> None
         | Right (SCons(s, vp)) -> eval vp (Some s) // `s` is possibly consumed by `LRelax`

let rec relaxPattern (relaxers: Relaxer list) (pattern: Pattern) : Pattern option =
    match relaxers with
    | [] -> Some pattern
    | r :: rs -> evalRelaxer r pattern ?|>? relaxPattern rs

// Check if two patterns are consistent, say they have exactly the same structures
// excluding the const strings.
let rec checkConsistency (pattern1: Pattern) (pattern2: Pattern) : bool =
    let rec onV (pattern1: VPattern) (pattern2: VPattern) : bool =
        match pattern1, pattern2 with
        | VNil, VNil -> true
        | VCons(v1, sp1), VCons(v2, sp2) ->
            if v1 = v2 then onS sp1 sp2 else false // check pvars
        | _ -> false
    and onS (pattern1: SPattern) (pattern2: SPattern) : bool =
        match pattern1, pattern2 with
        | SNil, SNil -> true
        | SCons(_, vp1), SCons(_, vp2) -> onV vp1 vp2 // ignore const strings
        | _ -> false

    match pattern1, pattern2 with
    | Left vp1, Left vp2 -> onV vp1 vp2
    | Right sp1, Right sp2 -> onS sp1 sp2
    | _ -> false

let checkConsistencyMany (patterns: Pattern list) : bool =
    match patterns with
    | [] -> true
    | p :: ps -> List.forall (checkConsistency p) ps

// Match a string with a pattern. Extract interesting variables.
let matchPattern (pattern: Pattern) (str: string) : PEnv option =
    let tryInsert (v: PVar) (str: string) (env: PEnv) : PEnv option =
        if env.ContainsKey(v)
        then let value = env.[v] in if str = value then Some env else None
        else Some (insert v str env)
    
    let rec onV (pattern: VPattern) (str: string) (env: PEnv) : PEnv option =
        match pattern with
        | VNil -> // expected str = ""
            if str = "" then Some env else None
        | VCons(v, SNil) -> // expected str = _, match `v` with all the rest
            tryInsert v str env
        | VCons(v, SCons(s, VNil)) -> // expected str = s' + s, `s` is tail and match `v` with `s'`
            if str.EndsWith(s)
            then tryInsert v str.[0..(str.Length - s.Length - 1)] env
            else None
        | VCons(v, SCons(s, vp)) -> // expected str = s' + s + _, search `s` and match `v` with `s'`
            if s = "" // we simply let `v` be the first char
            then tryInsert v str.[0..0] env ?|>? onV vp (str.Substring(1))
            else match str.IndexOf(s) with
                 | -1 -> None // `s` is not found in `str`, match fails
                 | k -> // str[0..k - 1] = s', store `v -> s'` in `env`
                     tryInsert v str.[0..k - 1] env ?|>? onV vp (str.Substring(k + s.Length))

    match pattern with
    | Left vp -> onV vp str Map.empty
    | Right SNil -> // expected str = ""
        if str = "" then Some Map.empty else None
    | Right (SCons(s, vp)) -> // expected str = s + _
        if str.StartsWith(s) then onV vp (str.Substring(s.Length)) Map.empty else None

// Apply a `env` on `pattern`, 
// substitute the variables in `pattern` with concrete strings in `env`.
// ASSUME every variable in the `pattern` must exist in `env`,
// otherwise this function throws an exception.
let applyPEnv (pattern: Pattern) (env: PEnv) : string =
    let rec onV (pattern: VPattern) : string =
        match pattern with
        | VNil -> ""
        | VCons(v, SNil) -> find v env
        | VCons(v, SCons(s, vp)) -> (find v env) + s + (onV vp)

    match pattern with
    | Left vp -> onV vp
    | Right SNil -> ""
    | Right (SCons(s, vp)) -> s + (onV vp)

(* Entry point of synthesis. *)

type SExample = string * string list * string

let isReservedWord (s: string) : bool = List.contains s ["a"; "an"; "is"; "are"; "be"; "in"]

type FullMatcher = Matcher * string

type FullPattern = FullMatcher list

// Given user-provided `examples` (after tokenized),
// synthesize the err pattern.
let synthesizeErrPattern (examples: SExample list) : ErrPattern =
    let syn (example: SExample): FullPattern =
        let eq, err, fix = example
        let rec gen (tokens: string list) (i: int) (his: Map<string, VarId>) (p: FullPattern) =
            match tokens with
            | [] -> p
            | e :: ts ->
                let m, his' =
                    let h = insert e i his in
                    let c = (Str(e), e), his in
                    if (eq.Contains(e) || fix.Contains(e)) && (not (isReservedWord e)) 
                    then match findByKey (fun (s: string) -> s.Contains(e)) his with
                         | Some (s, j) -> if s = e then (Var(j), e), h else c
                         | None -> (Var(i), e), h
                    else c

                gen ts (i + 1) his' (p |+| m)

        gen err 0 Map.empty []

    let unify (m1: FullMatcher) (m2: FullMatcher) : FullMatcher =
        match m1, m2 with
        | (Str(s1), _), (Str(s2), _) -> if s1 = s2 then (Str(s1), s1) else (Any, "")
        | (Str(s1), _), (Var(_), s2) -> if s1 = s2 then (Str(s1), s1) else (Any, "")
        | (Var(_), s1), (Str(s2), _) -> if s1 = s2 then (Str(s1), s1) else (Any, "")
        | (Var(i), _), (Var(_),_) -> (Var(i), "")
        | (Any, _), _ -> (Any, "")
        | _ -> (Any, "")

    let refine (pattern: FullPattern) (example: SExample) : FullPattern =
        let p = syn example
        if pattern.Length <> p.Length 
        then Log.Debug("refine: length not same: {0}, {1}", showList pattern, showList p)
             let n = min pattern.Length p.Length
             List.map2 unify (List.take n pattern) (List.take n p)
        else List.map2 unify pattern p

    match examples with
    | [] -> invalidArg "examples" "at least one example must be provided"
    | e :: es ->
        List.fold refine (syn e) es
        |> List.map fst

// Given an `errPattern` (synthesized by `synthesizeErrPattern`)
// and user-provided `examples` (after tokenized),
// synthesize relaxers if necessary.
let synthesizeRelaxers (errPattern: ErrPattern) (examples: SExample list) : Relaxer list option =
    let rec syn (acc: Relaxer list) (examples: (Pattern * string) list) : Relaxer list =
        match examples with
        | [] -> acc
        | (eqPattern, fix) :: es ->
            let eqPattern' = (relaxPattern acc eqPattern).Value
            let fixPattern = genRelaxedPattern eqPattern' fix
            let relaxers = extractRelaxers fixPattern
            syn relaxers es

    examples ||>? (fun (eq, err, fix) -> matchErr errPattern err ?|> (fun env -> (eq, env, fix)))
            ?||>  (fun (eq, env, fix) -> (genPattern eq env, fix))
            //   |>  assume "Inconsistent patterns." (List.unzip >> fst >> checkConsistencyMany)
            ?|>   syn []

// A schema/signature for a string transformer : v * vs -> v',
// identifying which values the from `env` shall be used as input parameters of the transformer.
// `v` is a `PVar` as the first parameter, and `vs` (a list of `VarId`) are _extra inputs_:
// we treat (simple) variables that are extracted from err message
// but unused in the pattern of input equation as _extra inputs_.
// They may be useful because they present in the fixed equation.
// Since string transformer is able to decide whether these extra inputs are really useful or not
// for a specific string transformation problem,
// it is safe to always include all the extra inputs for each string transformation problem.
type Schema = PVar * VarId list

// An input-output example for the string transformer,
// which takes a list of string as input and returns a string.
type TExample = string list * string

// Given an `errPattern` (synthesized by `synthesizeErrPattern`),
// `relaxers` (synthesized by `synthesizeRelaxers`),
// and user-provided `examples` (after tokenized),
// generate input-output examples as the input of string transformer.
// For each rule, several string transformers are needed.
// A map from `Schema` to `TExample` is returned to identify the input-output examples for each
// string transformer with a specific `Schema` as its signature.
let genTExamples (errPattern: ErrPattern) (relaxers: Relaxer list)
                 (examples: SExample list) : Map<Schema, TExample list> option =
    let rec PVarContains (target: VarId) (pVar: PVar) : bool =
        match pVar with
        | EnvVar(v) -> v = target
        | LRelaxVar(v) -> PVarContains target v
        | RRelaxVar(v) -> PVarContains target v
        | BRelaxVar(v1, v2) -> PVarContains target v1 || PVarContains target v2

    let PEnvContains (target: VarId) (pEnv: PEnv) : bool =
        Map.tryFindKey (fun k -> fun _ -> PVarContains target k) pEnv |> Option.isSome

    let gen (eq, err, fix) : Map<Schema * VarId list, TExample * string list> option =
        let env = (matchErr errPattern err).Value
        let p = genPattern eq env
        Log.Debug("Gen pattern: {0}", p)
        let p' = (relaxPattern relaxers p).Value
        Log.Debug("Relaxed pattern: {0}", p')
        // FIXME: this code is too messy
        match (matchPattern p' eq) with
        | Some(eqEnv) -> match (matchPattern p' fix) with
                         | Some(fixEnv) ->
                            let extra = Map.filter (fun k -> fun _ -> not (Map.containsKey (EnvVar k) eqEnv)) env
                            // extra inputs
                            let extraVars, extraVals = extra |> Map.toList |> List.unzip
                            // primitive inputs : may be used by `optimise`.
                            let primVars, primVals = env |> Map.toList |> List.unzip

                            eqEnv <*> fixEnv
                            |> mapKeysAndValues (
                                fun (k, (input, output)) -> (((k, extraVars), primVars), 
                                                             ((input :: extraVals, output), primVals)))
                            |> Some
                          | None -> None
        | None -> None
    
    let optimise ((eS, eEs) : (Schema * VarId list) * (TExample * string list) list) =
        let schema = fst eS
        let examples = List.map fst eEs
        let dedupExamples = List.distinct examples // 1. deduplicate
        
        // 2. check inconsistent, i.e. same input but different output
        if List.exists (fun (_, es) -> List.length es >= 2) (List.groupBy fst dedupExamples)
        then Log.Debug("Find inconsistent Texamples: {0}", showList dedupExamples) 
             ((fst schema, snd schema @ snd eS), 
              List.map (fun ((i, o), p) -> (i @ p, o)) eEs)
        else (schema, dedupExamples)

    examples ||>? gen ?|> collectMap ?|> (mapKeysAndValues optimise)
