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

for num in numSet do
    if found then ()
    if num > TARGET then ()
    let pair = TARGET - num
    if numSet.Contains pair then
        found <- true
        first <- num
        second <- pair
    ()

let answer = first * second

printfn "The answer is %d, from entries %d and %d" answer first second
