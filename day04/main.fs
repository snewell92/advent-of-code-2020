module Main

open System.IO


let getSeqFromInputFile (inputFile: string) =
    seq {
        let sr = new StreamReader(inputFile)
        while not sr.EndOfStream do
            yield sr.ReadLine()
        yield ""
        sr.Close()
    }

[<EntryPoint>]
let start argv =
    match Array.toList argv with
    | inputFile :: _ ->
        printfn "Using input file %s" inputFile
        inputFile
        |> getSeqFromInputFile
        |> Fields.processLines
        |> List.filter snd
        |> List.length
        |> printfn "There are %d valid passports"
        0
    | _ ->
        printfn "What input file should I use?"
        1
