open System.IO
open System.Collections.Generic

type Rule =
    | LeafRule of string
    | PartialRule of string * (string * int) list
    | InternalRule of string * (Rule * int) list

let extractChildRuleList rule =
    match rule with
    | LeafRule _ -> []
    | PartialRule _ -> []
    | InternalRule (_, childRules) -> childRules

let targetBag = "shiny gold"

let knownCache = Dictionary()
let ruleCache = Dictionary()

let extractName (str: string) =
    let idx = str.IndexOf("bags")
    str.Substring(0, idx - 1).Trim()

let extractChildRuleNames (str: string) =
    seq {
        let idx = str.IndexOf("contain") + 8
        let ruleParts = str.Substring(idx).Split(", ")

        for rule in ruleParts do
            let bagIdx = rule.IndexOf("bag")
            let num = rule.Substring(0, 1) |> int
            let rule = rule.Substring(2, bagIdx - 2).Trim()
            yield (rule, num)
    }

let buildRuleFromString (str: string) =
    let name = extractName str

    if str.Contains("contain no other bags") then
        let rule = LeafRule name
        ruleCache.Add(name, rule)
        knownCache.Add(name, rule)
        rule
    else
        let childRuleNames = extractChildRuleNames str |> List.ofSeq
        let rule = PartialRule(name, childRuleNames)
        ruleCache.Add(name, rule)
        rule

let allKnown =
    List.fold (fun known (name, _) -> known && knownCache.ContainsKey(name)) true

let fetchRules =
    List.map (fun (name, count) -> (knownCache.[name], count))

let mkInternal (name, childRules) =
    InternalRule(name, fetchRules childRules)

/// Converts as many partial rules to internal rules as possible
let passRules () =
    let mutable finished = true

    for kvp in ruleCache do
        match kvp.Value with
        | LeafRule _ -> ()
        | PartialRule (name, childRules) ->
            if allKnown childRules then
                let fullInternalRule = mkInternal (name, childRules)

                knownCache.Add(name, fullInternalRule)
                ruleCache.[name] <- fullInternalRule
                ()
            else
                finished <- false
                ()
        | InternalRule _ -> ()

    finished

let buildRules lineSeq =
    for line in lineSeq do
        buildRuleFromString line |> ignore

    let mutable finished = false

    while not finished do
        finished <- passRules ()

let rec calcRulesBagsCount rule adj =
    match rule with
    | LeafRule _ -> 1
    | PartialRule _ -> 1
    | InternalRule (_, children) ->
        adj
        + (children
           |> List.map (fun (rule, num) -> num * (calcRulesBagsCount rule 1))
           |> List.reduce (+))

let calcChildBags ruleName =
    calcRulesBagsCount ruleCache.[ruleName] 0

let inputToSeq (input: string) =
    seq {
        let sr = new StreamReader(input)

        while not sr.EndOfStream do
            yield sr.ReadLine()

        sr.Close()
    }

"input.txt" |> inputToSeq |> buildRules

printfn "%s must contain %d other bags" targetBag (calcChildBags targetBag)
