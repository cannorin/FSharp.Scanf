/// Scanf functions.
namespace FSharp.Scanf

open System
open System.IO
open FSharp.Scanf.Internals

[<AutoOpen>]
module Scanf =
  let inline ksscanf (pf: PrintfFormat<_,_,_,_,^t>) (cont: ^t -> 'u) s : 'u =
    let matches, formatters = Parser.getMatchesAndFormat pf s
    let strings = matches |> Seq.toList
    OptimizedConverter.convert strings formatters |> cont

  let inline tryKsscanf pf cont s =
    try
      ksscanf pf cont s |> Ok
    with
      | ex -> Error ex

  let inline sscanf pf s  =
    ksscanf pf id s

  let inline trySscanf pf s =
    tryKsscanf pf id s

  let inline scanfn pf =
    Console.ReadLine() |> sscanf pf

  let inline tryScanfn pf =
    Console.ReadLine() |> trySscanf pf

  let inline kscanfn pf cont =
    ksscanf pf cont <| Console.ReadLine()

  let inline tryKscanfn pf cont =
    tryKsscanf pf cont <| Console.ReadLine()

  let inline fscanfn pf (tr: TextReader) =
    tr.ReadLine() |> sscanf pf

  let inline tryFscanfn pf (tr: TextReader) =
    tr.ReadLine() |> trySscanf pf

  let inline kfscanfn pf cont (tr: TextReader) =
    ksscanf pf cont <| tr.ReadLine()

  let inline tryKfscanfn pf cont (tr: TextReader) =
    tryKsscanf pf cont <| tr.ReadLine()

  // active pattern
  let inline (|Sscanf|_|) (format:PrintfFormat<_,_,_,_,'t>) input =
    trySscanf format input |> function | Ok x -> Some x | Error _ -> None
