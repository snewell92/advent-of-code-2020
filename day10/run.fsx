#!/bin/dotnet fsi

open System.IO
open Jolts

let readInput (inputFile: string) =
    seq {
        let sr = new StreamReader(inputFile)

        while not sr.EndOfStream do
            yield sr.ReadLine() |> int64
    }

"sm-test-input.txt"
|> readInput
|> List.ofSeq
|> makeFinalAdapterList
|> calcNumOfValidPermutations
|> printfn "There are %d valid combinations"
