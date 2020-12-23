#!/bin/dotnet fsi

open System.IO
open System.Collections.Generic

let visitedLines = new HashSet<int>()

let visit = visitedLines.Add

let isVisited = visitedLines.Contains

type Command =
    | Noop
    | Acc
    | Jmp

type Instruction = Command * int

let parseCommand (str: string) =
    match str with
    | "nop" -> Noop
    | "acc" -> Acc
    | "jmp" -> Jmp
    | _ -> failwith <| sprintf "Unknown command %s" str

let parseInstruction (str: string): Instruction =
    let [| cmd; quantity |] = str.Split(' ').[..1]
    let parsedCmd = parseCommand cmd
    let amnt = quantity |> int
    (parsedCmd, amnt)

let linesFromFile (inputFile: string) =
    seq {
        let sr = new StreamReader(inputFile)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let getNextIndex instruction indexNum =
    match fst instruction with
    | Noop -> indexNum + 1
    | Acc -> indexNum + 1
    | Jmp -> indexNum + snd instruction

let getNextAccum currAccum currInstruction =
    match fst currInstruction with
    | Noop -> currAccum
    | Jmp -> currAccum
    | Acc -> currAccum + snd currInstruction

let fileName = "input.txt"

let program =
    fileName
    |> linesFromFile
    |> Seq.map parseInstruction
    |> List.ofSeq

let calcAccumTillRepeat () =
    let mutable accum = 0
    let mutable keepGoing = true
    let mutable currentInstructionIndex = 0

    while keepGoing do
        visit currentInstructionIndex |> ignore
        let currInstruction = program.[currentInstructionIndex]
        accum <- getNextAccum accum currInstruction
        currentInstructionIndex <- getNextIndex currInstruction currentInstructionIndex
        if isVisited currentInstructionIndex then keepGoing <- false

    accum

printfn "The accum is at %d right before the infinite loop starts for %s" (calcAccumTillRepeat ()) fileName
