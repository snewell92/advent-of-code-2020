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

let missingId =
    "input.txt"
    |> getSeqFromInputFile
    |> Seq.map (fun line -> (parseSeat line).id)
    |> List.ofSeq
    |> List.sort
    |> List.fold (fun (candidate, prev) seatId ->
        if prev = -1 then
            (candidate, seatId)
        else if candidate = -1 then
            let diff = seatId - prev
            if diff = 1 then (candidate, seatId) else (seatId - 1, seatId)
        else
            (candidate, prev)) (-1, -1)
    |> fst

printfn "My seat id is %d" missingId
