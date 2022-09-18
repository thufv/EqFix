module EqFix.Transformer

open EqFix.Lib.Transformer
open EqFix.Util

type TExample = string list * string

type TInput = string list

// F#: 'a seq = C#: IEnumerable<'a>
let transformInput : TInput -> string seq = List.toSeq

// NOTE remove same examples
let transformExamples (examples: TExample list) =
    List.map (fun (input, output) -> STExample(transformInput input, output)) examples

type ST = STProgram

type STVSA = ST list

type SolverName = PROSE = 1  // EqFix.Lib.Transformer.PROSETextTransformer
                | STLang = 2 // EqFix.Lib.Transformer.STLangTransformer

// Configuration for string transformer.
[<StructuredFormatDisplay("maxK: {maxK}, solver: {solver}")>]
type SolverConfig = { maxK: int; solver: SolverName }

// Internal wrapper of EqFix.Lib.Transformer.StringTransformer
type Solver (config: SolverConfig) =
    let st = StringTransformer.SetupST(config.solver.ToString())
    member this.K = config.maxK
    member this.Solve (examples: TExample list): STVSA option =
        let p = st.Synthesize(transformExamples examples, this.K) |> Seq.toList
        if List.isEmpty p then None else Some p
    member this.SolveTop (examples: TExample list): ST option =
        let p = st.Synthesize(transformExamples examples, this.K) |> Seq.toList
        if List.isEmpty p then None else Some (List.head p)