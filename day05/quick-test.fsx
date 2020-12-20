#!/usr/bin/dotnet fsi

#load "seating.fs"

open Seating
open System.IO

printfn "Starting up!"

let getSeqFromInputFile (inputFile: string) =
    seq {
        let sr = new StreamReader(inputFile)
        while not sr.EndOfStream do
            yield sr.ReadLine()
        sr.Close()
    }

let maxId =
    "input.txt"
    |> getSeqFromInputFile
    |> Seq.map (fun line -> (parseSeat line).id)
    |> Seq.max

printfn "The max id is %d" maxId
