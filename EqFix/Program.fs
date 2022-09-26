open EqFix.Util
open EqFix.Bench
open EqFix.Lib.Logging
open EqFix

open Argu
open System
open System.IO

type BatchOptions =
    | [<MainCommand; Mandatory>] CommandFile of file: string
with interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | CommandFile _ -> "the command file."

type CLIOptions =
    | [<AltCommandLine("-log")>] Log_Level of Logger.LogLevel
    | [<AltCommandLine("-p")>] Plain_Log
    | [<CliPrefix(CliPrefix.None)>] Bench of ParseResults<BenchOptions>
    | [<CliPrefix(CliPrefix.None)>] Batch of ParseResults<BatchOptions>
with interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Log_Level _ -> "specify the log level, default info."
            | Plain_Log _ -> "enable plain log mode (default colorful log)."
            | Bench _ -> "benchmarking."
            | Batch _ -> "execute interactive commands in one batch."

type StdInIterator() =
    interface Iterator<string> with
        member this.HasCurrent = true
        member this.GetCurrent = Console.ReadLine()
        member this.MoveNext() = ()

type StringListIterator(ss: string list) =
    let mutable l = ss
    interface Iterator<string> with
        member this.HasCurrent = not l.IsEmpty
        member this.GetCurrent = l.Head
        member this.MoveNext() = l <- l.Tail

[<EntryPoint>]
let main argv =
    let exe = "EqFix.exe"
    let errorHandler = ProcessExiter()
    let parser = ArgumentParser.Create<CLIOptions>(programName = exe, errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv

    // setup loggers
    Log.ShowColor <- not (results.Contains Plain_Log)
    Log.DisplayLevel <- results.GetResult (Log_Level, defaultValue = Logger.LogLevel.INFO)

    if results.Contains Bench then
        results.GetResult Bench |> bench
    else if results.Contains Batch then
        (results.GetResult Batch).GetResult CommandFile
        |> File.ReadAllLines
        |> List.ofArray |> StringListIterator
        |> REPL.repl false
    else
        StdInIterator () |> REPL.repl true

    0 // return an integer exit code
