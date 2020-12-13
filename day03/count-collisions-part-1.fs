open System
open System.IO

module CountCollision =
  type SquareType =
    | Tree
    | Open

  let charToSquare c =
    match c with
    | '.' -> Open
    | '#' -> Tree
    | _ ->
      c
      |> sprintf "Invalid sqaure %c"
      |> Exception
      |> raise

  let printSquare s =
    match s with
    | Tree -> "Tree"
    | Open -> "Open"

  /// Decodes str slope input into rise * run (will throw if bad input!)
  let decodeSlope (str: string) =
    let splits = str.Split('/')
    (splits.[0] |> int, splits.[1] |> int)

  /// Loads the given initial hunk of input in which each row repeats to the right
  let loadInput (inputFile: string) =
    let mutable chunk = []
    let sr = new StreamReader(inputFile)
    while not sr.EndOfStream do
      let line = sr.ReadLine()
      let mutable newRow = []
      for c in line do
        newRow <- List.append newRow [charToSquare c]
      chunk <- List.append chunk [newRow]
    chunk

  let getSquareFromChunk (chunk: SquareType list list) (x, y) =
    let row = chunk.[y]
    if x < row.Length then
      row.[x]
    else
      row.[(x % (row.Length))]
  
  let applySlope (x, y) (rise, run) =
    (x + run, y + rise)

  let countCollisions (initX, initY) (mapChunk: SquareType list list) slopeStr =
    let slope = decodeSlope slopeStr
    let mutable currCoord = (initX,initY)
    let mutable collisions = 0
    let maxY = mapChunk.Length-1
    let mutable keepGoing = true
    while keepGoing do
      if (snd currCoord) > maxY then
        keepGoing <- false
      else
        match getSquareFromChunk mapChunk currCoord with
        | Tree ->
          collisions <- collisions + 1
        | Open ->
          ()
        |> ignore
        currCoord <- applySlope currCoord slope
    collisions

let main argv =
  match Array.toList argv with
  | inputFile :: slope :: _ ->
    printfn "Using input file '%s' with slope %s" inputFile slope
    let zeChunk = CountCollision.loadInput inputFile
    printfn "  > There are %d collisions" (CountCollision.countCollisions (0, 0) zeChunk slope)
    0
  | _ ->
    printfn "Need an input file and slope as first two args"
    1

[<EntryPoint>]
let phaseTwo argv =
  match Array.toList argv with
  | inputFile :: _ ->
    let zeChunk = CountCollision.loadInput inputFile
    let answer =
      [
        "1/1"
        "1/5"
        "1/3"
        "1/7"
        "2/1"
      ]
      |> (List.fold (fun curr slope ->
        let collisions = CountCollision.countCollisions (0,0) zeChunk slope
        printfn "  > For slope %s found %d collisions" slope collisions
        curr * (collisions|>float)
      ) 1.0)
    printfn "Answer is %.0f" answer
    0
  | _ ->
    printfn "Need an input file and slope as first two args"
    1