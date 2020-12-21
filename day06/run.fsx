open System.IO
open System.Collections.Generic

type IndividualAnswers = char list

let toIndiv: char list -> IndividualAnswers = id

type GroupAnswers = IndividualAnswers list

type PlaneAnswers = GroupAnswers list

let uniq items =
    let cache = Dictionary()
    for item in items do
        cache.TryAdd(item, true) |> ignore
    cache.Keys |> List.ofSeq

let lineToIndiv: string -> IndividualAnswers = uniq >> toIndiv

let mkGroup: IndividualAnswers list -> GroupAnswers = id

let isEmpty (s: string): bool = s.Trim().Length = 0

let mkInputSeq (inputFile: string) =
    seq {
        let sr = new StreamReader(inputFile)
        let mutable indivs: IndividualAnswers list = []
        while not sr.EndOfStream do
            let line = sr.ReadLine()
            if isEmpty line then
                yield mkGroup indivs
                indivs <- []
            else
                indivs <- (lineToIndiv line) :: indivs
            ()
        yield mkGroup indivs
        sr.Close()
    }

let groupToSeq (ga: GroupAnswers) =
    seq {
        for indiv in ga do
            for ans in indiv do
                yield ans
    }

let uniqGroupAnswers: GroupAnswers -> char list = groupToSeq >> uniq

let countGroup ga = (uniqGroupAnswers ga).Length

let countWhereEveryoneAnswered (ga: GroupAnswers) =
    let targetCount = ga.Length
    let tracker = Dictionary()
    for indiv in ga do
        for ans in indiv do
            if tracker.ContainsKey(ans) then tracker.[ans] <- tracker.[ans] + 1 else tracker.Add(ans, 1)
    let mutable total = 0
    for kvp in tracker do
        if kvp.Value = targetCount then total <- total + 1
    total

let countAllInFile inputFile =
    inputFile
    |> mkInputSeq
    |> Seq.map countWhereEveryoneAnswered
    |> Seq.reduce (+)
