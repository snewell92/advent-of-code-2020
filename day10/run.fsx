#!/bin/dotnet fsi

open System.IO

let readInput (inputFile: string) =
    seq {
        let sr = new StreamReader(inputFile)

        while not sr.EndOfStream do
            yield sr.ReadLine() |> int
    }

let makeFinalAdapterList bag =
    let baggedThings = List.sort bag
    List.append baggedThings [ 3 + (List.last baggedThings) ]

let calcDiffs bag =
    let sortedBag = List.sort bag
    let mutable lastSeenJoltage = 0
    let mutable oneDiffs = 0
    let mutable twoDiffs = 0
    let mutable threeDiffs = 0

    for joltAdapter in sortedBag do
        match joltAdapter - lastSeenJoltage with
        | 1 -> oneDiffs <- oneDiffs + 1
        | 2 -> twoDiffs <- twoDiffs + 1
        | 3 -> threeDiffs <- threeDiffs + 1
        | _ -> failwithf "WOAH! wtf? I got %d but last saw %d???" joltAdapter lastSeenJoltage

        lastSeenJoltage <- joltAdapter

    (oneDiffs, twoDiffs, threeDiffs)

let (oneDiff, twoDiff, threeDiff) =
    "input.txt"
    |> readInput
    |> List.ofSeq
    |> makeFinalAdapterList
    |> calcDiffs

printfn "There are %d 1 jolt diffs, %d 2 jolt diffs, and %d 3 jolt diffs" oneDiff twoDiff threeDiff
printfn "The answer is %d" (oneDiff * threeDiff)
