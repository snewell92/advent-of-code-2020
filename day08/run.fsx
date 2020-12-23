#!/bin/dotnet fsi

open System.IO
open System.Collections.Generic

let visitedLines = new HashSet<int>()

let visit = visitedLines.Add

let isVisited = visitedLines.Contains

let reset = visitedLines.Clear

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

let originalProgram =
    fileName
    |> linesFromFile
    |> Seq.map parseInstruction
    |> List.ofSeq

let runProgram (program: Instruction list) =
    let mutable accum = 0
    let mutable infiniteLoop = false
    let mutable keepGoing = true
    let mutable currentInstructionIndex = 0

    while keepGoing do
        visit currentInstructionIndex |> ignore
        let currInstruction = program.[currentInstructionIndex]
        accum <- getNextAccum accum currInstruction
        currentInstructionIndex <- getNextIndex currInstruction currentInstructionIndex

        if isVisited currentInstructionIndex then
            infiniteLoop <- true
            keepGoing <- false

        if currentInstructionIndex >= program.Length
           || currentInstructionIndex <= 0 then
            keepGoing <- false

    reset ()
    (accum, infiniteLoop)

let flip ins =
    match ins with
    | Noop -> (Jmp, true)
    | Jmp -> (Noop, true)
    | Acc -> (Acc, false)

let mutateProgram (idx: int) (newCmd: Command) =
    List.mapi (fun i el -> if i = idx then (newCmd, snd el) else el)

let genNewPrograms (program: Instruction list): (Instruction list) seq =
    seq {
        for i in 0 .. (program.Length - 1) do
            match flip <| fst program.[i] with
            | (x, true) -> yield mutateProgram i x program
            | _ -> ()
    }

let findAnswer () =
    let mutable stop = false
    let mutable answer = -1

    for candidate in genNewPrograms originalProgram do
        if stop then ()
        let (finalAccum, wasInfinite) = runProgram candidate

        if not wasInfinite then
            answer <- finalAccum
            stop <- true

        ()

    answer

printfn "The final, corrected, accum is %d for %s" (findAnswer ()) fileName
