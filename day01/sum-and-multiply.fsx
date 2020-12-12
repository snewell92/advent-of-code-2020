#!/usr/bin/dotnet fsi

open System.IO

let INPUT_FILE = "input.txt"
let TARGET = 2020

let lines = File.ReadAllLines(INPUT_FILE)
let inputLen = lines.Length

if inputLen = 0 then
    printfn "No lines of input read in, no answer!"
    exit 0


printfn "Read in %d lines of input" inputLen

let inputReducer =
    fun r -> Array.fold r (false, (-1, -1)) lines

let (_, (first, second)) =
    inputReducer (fun (found, ans) outer ->
        if found then
            (found, ans)
        else
            let outerInt = outer |> int
            inputReducer (fun (found, ans) inner ->
                if found then
                    (found, ans)
                else
                    let innerInt = inner |> int
                    if outerInt + innerInt = TARGET then (true, (outerInt, innerInt)) else (found, ans)))

let answer = first * second

printfn "The answer is %d, from entries %d and %d" answer first second
