#!/usr/bin/dotnet fsi

open System.IO


let INPUT_FILE = "input.txt"

/// Converts a list of characters into a string
let charsToStr = List.toArray >> System.String

/// Converts a list of characters into an int (throws if the string is not an int!)
let charsToInt = charsToStr >> int

let isDash c = c = '-'

let isSpace c = c = ' '

let DIGITS = [ '0' .. '9' ]

let isDigit c = List.contains c DIGITS

type PasswordInput =
    { FirstIdx: int
      SecondIdx: int
      TargetChar: char
      Password: char list }

let mkPI first second char pass =
    { FirstIdx = first
      SecondIdx = second
      TargetChar = char
      Password = pass }

let showPI pi =
    sprintf
        "For %s, expected %c either at position %d or %d"
        (charsToStr pi.Password)
        pi.TargetChar
        pi.FirstIdx
        pi.SecondIdx

let isValid (pi: PasswordInput) =
    (pi.Password.[pi.FirstIdx] = pi.TargetChar)
    <> (pi.Password.[pi.SecondIdx] = pi.TargetChar)


/// Parses a char list recursively, extracting an int, ending on some criteria specified by target
let rec getIntTillRec str target buf =
    match str with
    | c :: rest ->
        if target c
        then ((charsToInt buf) - 1, rest)
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
