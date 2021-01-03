open Jolts
open System.IO

let readInput (inputFile: string) =
    seq {
        let sr = new StreamReader(inputFile)

        while not sr.EndOfStream do
            yield sr.ReadLine() |> int64
    }

[<EntryPoint>]
let main args =
  printfn "Using file %s" args.[0]

  args.[0]
  |> readInput
  |> List.ofSeq
  |> makeFinalAdapterList
  |> calcNumOfValidPermutations
  |> printfn "There are %d valid combinations"

  0
