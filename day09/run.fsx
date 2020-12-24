#!/bin/dotnet fsi

open System.IO

/// The only number that doesn't obey the rule (from part 1)
// let TARGET_NUM = 127L
let TARGET_NUM = 257342611L

let iterateListTillFound (list: 'a list) f =
    let mutable answer = None
    let mutable idx = 0

    while idx < list.Length && Option.isNone answer do
        answer <- f (list.[idx])
        idx <- idx + 1

    answer

let shiftStackList (nums: int64 list) newNum =
    newNum
    :: nums.GetSlice(Some 0, Some <| nums.Length - 2)

let findTargetFromSum (nums: int64 list) target =
    iterateListTillFound
        nums
        (fun x -> iterateListTillFound nums (fun y -> if x + y = target then Some(x, y) else None))

let readInput (inputFile: string) =
    seq {
        let sr = new StreamReader(inputFile)

        while not sr.EndOfStream do
            yield sr.ReadLine() |> int64

        sr.Close()
    }

let findMaxAndMin (list: int64 list) = (List.max list, List.min list)

let addPair (x: int64, y: int64) = x + y

let getSublist x y list =
    list |> List.skip x |> List.take (y - x)

let addSublist x y (list: int64 list) = list |> getSublist x y |> List.sum

let findSublist target list =
    let mutable candidateList = []
    let mutable startIdx = 0
    let mutable endIdx = 1
    let mutable keepGoing = true

    while keepGoing do
        let currSublistSum = addSublist startIdx endIdx list

        if currSublistSum = target then
            keepGoing <- false
            candidateList <- getSublist startIdx endIdx list
        else if currSublistSum > target then
            startIdx <- startIdx + 1
            endIdx <- startIdx + 1
        else
            endIdx <- endIdx + 1

    candidateList

readInput "input.txt"
|> List.ofSeq
|> findSublist TARGET_NUM
|> findMaxAndMin
|> addPair
|> printfn "The weakness is %d"
