#!/usr/bin/dotnet fsi

open System.IO
open System.Collections.Generic


let INPUT_FILE = "input.txt"
let TARGET = 2020

let numSet = new HashSet<int>()

let sr = new StreamReader(INPUT_FILE)

while not sr.EndOfStream do
    let succeeded = () |> sr.ReadLine |> int |> numSet.Add
    if not succeeded then
        printfn "Something happened while reading data, got to #%d" numSet.Count
        exit 1


if numSet.Count = 0 then
    printfn "No lines of input read in, no answer!"
    exit 0


printfn "Read in %d lines of input" numSet.Count


let mutable found = false
let mutable first = -1
let mutable second = -1
let mutable third = -1

for firstCandidate in numSet do
    for secondCandidate in numSet do
        if found then ()
        if firstCandidate + secondCandidate > TARGET
        then ()

        let expectedThird =
            TARGET - (firstCandidate + secondCandidate)

        if numSet.Contains expectedThird then
            found <- true
            first <- firstCandidate
            second <- secondCandidate
            third <- expectedThird
        else
            ()


let answer = first * second * third

printfn "The answer is %d %d*%d*%d" answer first second third
