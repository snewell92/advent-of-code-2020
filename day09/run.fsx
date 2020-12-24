#!/bin/dotnet fsi

open System.IO

let PREAMBLE_LEN = 25

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

let input = readInput "input.txt"

let mutable preamble =
    Seq.take PREAMBLE_LEN input
    |> Seq.rev
    |> List.ofSeq

let nums = Seq.skip PREAMBLE_LEN input

for n in nums do
    if Option.isNone <| findTargetFromSum preamble n
    then printfn "The first number to violate the rule is %d" n

    preamble <- shiftStackList preamble n
