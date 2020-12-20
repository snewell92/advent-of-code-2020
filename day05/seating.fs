module Seating

type ColumnSpec =
    | L
    | R

type RowSpec =
    | F
    | B

type SeatSpec =
    | RowSpec of RowSpec
    | ColumnSpec of ColumnSpec

let rowSpecToStr rs =
    match rs with
    | F -> "F"
    | B -> "B"

let colSpecToStr cs =
    match cs with
    | L -> "L"
    | R -> "R"

let seatSpecToStr ss =
    match ss with
    | RowSpec rs -> rowSpecToStr rs
    | ColumnSpec cs -> colSpecToStr cs

let specsToStr specs =
    List.map seatSpecToStr specs |> String.concat ""

let toRow seat =
    match seat with
    | RowSpec rs -> rs
    | ColumnSpec _ -> failwith "Invalid row seat"

let toRows = List.map toRow

let toCol seat =
    match seat with
    | RowSpec _ -> failwith "Invalid col seat"
    | ColumnSpec cs -> cs

let toCols = List.map toCol

let splitSpecifiers (specifiers: SeatSpec list): (RowSpec list * ColumnSpec list) =
    let (rows, cols) = List.splitAt 7 specifiers
    (toRows rows, toCols cols)

type Seat =
    { boardingPassEntry: string
      rowSpec: RowSpec list
      colSpec: ColumnSpec list
      row: int
      column: int
      id: int }

let charToSpec c =
    match c with
    | 'L' -> ColumnSpec L
    | 'R' -> ColumnSpec R
    | 'F' -> RowSpec F
    | 'B' -> RowSpec B
    | _ -> failwith "Invalid seat spec, bad boarding pass"

let parseSpec (bpStr: string) =
    List.ofSeq bpStr
    |> List.fold (fun specs char -> List.append specs [ charToSpec char ]) []

let getHalf l h = (h + l) / 2

let rec calcRow (rowSpecs: RowSpec list) (low, high) =
    match rowSpecs with
    | rs :: rest ->
        let half = getHalf low high
        match rs with
        | F -> calcRow rest (low, half)
        | B -> calcRow rest (half + 1, high)
    | [] -> (low, low)

let rec calcCol (colSpecs: ColumnSpec list) (low, high) =
    match colSpecs with
    | cs :: rest ->
        let half = getHalf low high
        match cs with
        | L -> calcCol rest (low, half)
        | R -> calcCol rest (half + 1, high)
    | [] -> (low, low)

let calcId row col = row * 8 + col

let parseSeat seatLine =
    let specs = parseSpec seatLine
    let (rows, cols) = splitSpecifiers specs
    let rowNum = fst <| calcRow rows (0, 127)
    let colNum = fst <| calcCol cols (0, 7)
    { boardingPassEntry = seatLine
      rowSpec = rows
      colSpec = cols
      row = rowNum
      column = colNum
      id = calcId rowNum colNum }
