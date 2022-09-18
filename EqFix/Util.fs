module EqFix.Util

open EqFix.Lib.Logging
let Log = Logger.Instance

let LEVEL_FINE = Logger.LogLevel.FINE
let LEVEL_DEBUG = Logger.LogLevel.DEBUG
let LEVEL_INFO = Logger.LogLevel.INFO

(* Useful functions to manipulate functions that may be failed *)

let (?|>) (input: 't option) (next: 't -> 'v) : 'v option =
    Option.map next input

let (?|>?) (input: 't option) (next: 't -> 'v option) : 'v option =
    match input with
    | Some t -> next t
    | None -> None

let rec (||>) (inputs: 't list) (next: 't -> 'v) : 'v list =
    List.map next inputs

let rec (||>?) (inputs: 't list) (next: 't -> 'v option) : 'v list option =
    match inputs with
    | [] -> Some []
    | x :: xs -> match next x with
                 | Some y -> match xs ||>? next with
                             | Some ys -> Some (y :: ys)
                             | None -> None
                 | None -> None

let (?||>?) (inputs: 't list option) (next: 't -> 'v option) : 'v list option =
    match inputs with
    | Some is -> is ||>? next
    | None -> None

let (?||>) (inputs: 't list option) (next: 't -> 'v) : 'v list option =
    match inputs with
    | Some is -> Some (is ||> next)
    | None -> None

let assume (errMsg: string) (test: 't -> bool) (input : 't option): 't option =
    Option.bind (fun x -> if test x then Some x else Log.Error(errMsg); None) input

(* Useful functions on lists *)

let (|+|) (xs: 'a list) (y: 'a) : 'a list = xs @ [y]

let deduplicate xs =
    let rec mem xs x =
        match xs with
        | [] -> false
        | head :: tail -> if x = head then true else mem tail x

    let rec removeduprec list1 list2 =
        match list1 with
        | [] -> list2
        | head :: tail when not (mem list2 head) -> removeduprec tail (head::list2)
        | h::t -> removeduprec t list2
     
    removeduprec xs [] |> List.rev

let zipIndexes (xs: 'a list) = List.zip [0..xs.Length - 1] xs

let rec flatMapOption (f: 'a -> 'b list option) (xs: 'a list) : 'b list option =
    match xs with
    | [] -> Some []
    | x :: xs' ->
        f x |> Option.bind (fun y -> flatMapOption f xs' |> Option.map (fun y' -> y @ y'))

let cartesian (xs: 'a list) (ys: 'b list) (f: 'a -> 'b -> 'c) : 'c seq =
    Seq.collect (fun x -> Seq.map (f x) ys) xs

let rec lazyProduct (xss: 'a seq list): 'a list seq =
    match xss with
    | [] -> Seq.empty
    | [xs] -> Seq.map (fun x -> [x]) xs
    | xs :: yss -> Seq.collect (fun x -> Seq.map (fun ys -> x :: ys) (lazyProduct yss)) xs

let rec findAndReturn (test: 'a -> 'b option) (xs: 'a seq): 'b option =
    if Seq.isEmpty xs then None
    else match test (Seq.head xs) with
         | Some r -> Some r
         | None -> findAndReturn test (Seq.tail xs)

(* Useful functions on maps *)

let insert (key: 'K) (value: 'V) (map: Map<'K, 'V>) = Map.add key value map

let find (key: 'K) (map: Map<'K, 'V>) = Map.find key map

let keys (map: Map<'K, 'V>) : 'K Set =
    map |> Map.toSeq |> Seq.map fst |> Set

let values (map: Map<'K, 'V>) : 'V list =
    map |> Map.toList |> List.map snd

let mapValues (f: 'V -> 'U) (map: Map<'K, 'V>): Map<'K, 'U> = 
    Map.map (fun _ -> f) map

let tryMapValues (f: 'V -> 'U option) (map: Map<'K, 'V>): Map<'K, 'U> option = 
    let folder acc k v = match acc with
                         | Some m -> match f v with
                                     | Some u -> Some (insert k u m)
                                     | None -> None
                         | None -> None
    
    Map.fold folder (Some Map.empty) map

let mapKeysAndValues (f: 'K * 'V -> 'J * 'U) (map: Map<'K, 'V>) : Map<'J, 'U> =
    map |> Map.toList
        |> List.map f
        |> Map

let (<*>) (map1: Map<'K, 'V1>) (map2: Map<'K, 'V2>): Map<'K, 'V1 * 'V2> =
    let keys2 = keys map2
    map1 |> Map.filter (fun k -> fun _ -> keys2.Contains(k))
         |> Map.map (fun k -> fun v1 -> (v1, Map.find k map2))

let findByKey (p: 'K -> bool) (map: Map<'K, 'V>): ('K * 'V) option =
    Map.tryFindKey (fun k -> fun _ -> p k) map |> Option.map (fun k -> (k, Map.find k map))

let findMany (ks: 'K list) (map: Map<'K, 'V>): 'V list =
    List.map (fun k -> Map.find k map) ks

let tryFindMany (ks: 'K list) (map: Map<'K, 'V>): 'V list option =
    List.map (fun k -> Map.tryFind k map) ks ||>? id

let collectMap (maps: Map<'a, 'b> list) : Map<'a, 'b list> =
    match maps with
    | [] -> Map.empty
    | m :: _ -> Map.map (fun k -> fun _ -> List.map (Map.find k) maps) m

let uncollectMap (map: Map<'a, 'b seq>) : Map<'a, 'b> seq =
    map |> Map.toList
        |> List.map (fun (l, xs) -> Seq.map (fun x -> (l, x)) xs)
        |> lazyProduct
        |> Seq.map Map

(* Either monad *)
[<StructuredFormatDisplay("{AsString}")>]
type Either<'L, 'R> = Left of 'L 
                    | Right of 'R
with override this.ToString() =
                    match this with
                    | Left l -> l.ToString()
                    | Right r -> r.ToString()
     member this.AsString = this.ToString()

(* String utility *)

// An index range `(l, r)` = `[l..r]`, including `l` and `r`.
type PosRange = int * int

// Find all occurences of string `target` in `s`.
// NOTE the returned occurences are not overlapped.
let searchString (s: string) (target: string) : PosRange list =
    let n = target.Length
    let rec search (offset: int) (acc: PosRange list) =
        match s.IndexOf(target, offset) with
        | -1 -> acc
        | l -> search (l + n) (acc |+| (l, l + n - 1))
    
    if n = 0 then [] else search 0 []

(* Logger *)

let showList xs =
    sprintf "[ %s ]" (String.concat ", " (List.map (sprintf "%A") xs))

let showMap map =
    sprintf "{ %s }" (String.concat ",\n  "
        (List.map (fun (k, v) -> sprintf "%A -> %A" k v) (Map.toList map)))



open System.Diagnostics

let measureRuntime f label =
    let stopWatch = Stopwatch.StartNew()
    let result = f ()
    stopWatch.Stop()
    Log.Info("{0} time: {1} ms", label, stopWatch.Elapsed.TotalMilliseconds)
    result

let show x =
    printfn "%A" x
    x