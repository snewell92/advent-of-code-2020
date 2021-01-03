module Jolts

open System.Collections.Generic

let makeFinalAdapterList bag =
    let baggedThings = List.sort bag
    0L :: baggedThings

/// Given an index and its value, returns its list of successors if able
let calcSuccessorsAtIndex (sortedBag: int64 list) (index, currNum) =
    let mutable choices = []
    let mutable idx = index + 1
    let mutable keepGoing = idx < sortedBag.Length

    if not keepGoing then
        []
    else
        while keepGoing do
            if (sortedBag.[idx] - currNum) <= 3L then
                choices <- (idx, sortedBag.[idx]) :: choices
            else
                keepGoing <- false

            idx <- idx + 1

            if idx >= sortedBag.Length then
                keepGoing <- false

        choices

let rememberHit (cache: Dictionary<int, int64>) k v = cache.Add(k, v)

let checkCache (cache: Dictionary<int, int64>) k =
    if cache.ContainsKey(k) then
        Some cache.[k]
    else
        None

let rec calcHitsRec (cache: Dictionary<int, int64>) (sortedBag: int64 list) (target: int64) (idx: int) =
    calcSuccessorsAtIndex sortedBag (idx, sortedBag.[idx])
    |> List.fold
        (fun count (successorIndex, successor) ->
            match checkCache cache successorIndex with
            | None ->
                if successor = target then
                    rememberHit cache successorIndex 1L
                    count + 1L
                else
                    let c =
                        calcHitsRec cache sortedBag target successorIndex

                    rememberHit cache successorIndex c
                    count + c
            | Some c -> count + c)
        0L

let calcNumOfValidPermutations (sortedBag: int64 list) =
    calcHitsRec (Dictionary<int, int64>()) sortedBag (List.last sortedBag) 0
