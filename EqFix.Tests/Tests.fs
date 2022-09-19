module Tests

open System
open Xunit

open EqFix.Synthesizer
open EqFix.Transformer

let eq1 = "+ alpha"
let err1 = "alpha is a letter, + shall be -"
let fix1 = "- \\alpha"
let example1 = (eq1, err1, fix1)

let eq2 = "- beta"
let err2 = "beta is a letter, - shall be +"
let fix2 = "+ \\beta"
let example2 = (eq2, err2, fix2)

let config = { maxK = 1; solver = SolverName.STLang }

[<Fact>]
let ``example 1 and 2 are consistent`` () =
    synthesizeRule config [example1; example2] |> Option.isSome |> Assert.True

[<Fact>]
let ``learning by example 1 is sound`` () =
    match synthesizeRulesAndTest config ([example1], [example1]) with
    | Some (Some k :: _) -> Assert.Equal(1, k)
    | _ -> Assert.True(false)

[<Fact>]
let ``learning by example 2 is sound`` () =
    match synthesizeRulesAndTest config ([example2], [example2]) with
    | Some (Some k :: _) -> Assert.Equal(1, k)
    | _ -> Assert.True(false)

[<Fact>]
let ``learning by example 1 and 2 is sound`` () =
    match synthesizeRulesAndTest config ([example1; example2], [example1; example2]) with
    | Some (Some k1 :: Some k2 :: _) ->
        Assert.Equal(1, k1)
        Assert.Equal(1, k2)
    | _ -> Assert.True(false)

[<Fact>]
let ``the rule learned by example 1 applies to example 2`` () =
    match synthesizeRulesAndTest config ([example1], [example2]) with
    | Some (Some k :: _) -> Assert.Equal(1, k)
    | _ -> Assert.True(false)

[<Fact>]
let ``the rule learned by example 2 applies to example 1`` () =
    match synthesizeRulesAndTest config ([example2], [example1]) with
    | Some (Some k :: _) -> Assert.Equal(1, k)
    | _ -> Assert.True(false)
