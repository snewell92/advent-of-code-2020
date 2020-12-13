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

  let countCollisions (initX, initY) inputFile slopeStr =
    let slope = decodeSlope slopeStr
    let mapChunk = loadInput inputFile
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

[<EntryPoint>]
let main argv =
  match Array.toList argv with
  | inputFile :: slope :: _ ->
    printfn "Using input file '%s' with slope %s" inputFile slope
    printfn "  > There are %d collisions" (CountCollision.countCollisions (0, 0) inputFile slope)
    0
  | _ ->
    printfn "Need an input file and slope as first two args"
    1
