#!/usr/bin/dotnet fsi

open System.IO


let INPUT_FILE = "input.txt"

let numOfOccurences (targetChar: char) (str: char list): int =
    let mutable count = 0
    for c in str do
        if c = targetChar then count <- count + 1
    count

/// Converts a list of characters into a string
let charsToStr = List.toArray >> System.String

/// Converts a list of characters into an int (throws if the string is not an int!)
let charsToInt = charsToStr >> int

let isDash c = c = '-'

let isSpace c = c = ' '

let DIGITS = [ '0' .. '9' ]

let isDigit c = List.contains c DIGITS

type PasswordInput =
    { MinOccurence: int
      MaxOccurence: int
      TargetChar: char
      Password: char list }

let mkPI minOcc maxOcc char pass =
    { MinOccurence = minOcc
      MaxOccurence = maxOcc
      TargetChar = char
      Password = pass }

let showPI pi =
    sprintf
        "For %s, expect %c between %d-%d times"
        (charsToStr pi.Password)
        pi.TargetChar
        pi.MinOccurence
        pi.MaxOccurence

let isValid (pi: PasswordInput) =
    let num =
        numOfOccurences pi.TargetChar pi.Password

    num >= pi.MinOccurence && num <= pi.MaxOccurence


/// Parses a char list recursively, extracting an int, ending on some criteria specified by target
let rec getIntTillRec str target buf =
    match str with
    | c :: rest ->
        if target c
        then (charsToInt buf, rest)
        else getIntTillRec rest target (List.append buf [ c ])
    | _ -> (-1, str)

/// Consumes the given char list, returning
let getIntTillDash str = getIntTillRec str isDash []
let getIntTillSpace str = getIntTillRec str isSpace []

let getCharTillColon str =
    match str with
    | c :: ':' :: ' ' :: rest -> (c, rest)
    | _ -> (' ', str)

let decodeLine (rawInput: string): PasswordInput =
    let (min, part1) = getIntTillDash (List.ofSeq rawInput)
    let (max, part2) = getIntTillSpace part1
    let (char, rest) = getCharTillColon part2
    mkPI min max char rest

let getPasswords () =
    seq {
        let sr = new StreamReader(INPUT_FILE)
        while not sr.EndOfStream do
            yield () |> sr.ReadLine |> decodeLine
    }

let numOfValidPasswords =
    ()
    |> getPasswords
    |> Seq.filter isValid
    |> Seq.countBy id
    |> Seq.length

printfn "There are %d valid passwords" numOfValidPasswords
