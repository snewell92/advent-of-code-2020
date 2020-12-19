module Fields

type KnownField =
    | BYR of string
    | IYR of string
    | EYR of string
    | HGT of string
    | HCL of string
    | ECL of string
    | PID of string
    | CID of string

type PassportField =
    | Known of KnownField
    | Unknown of string * string

let fieldToStr pf =
    match pf with
    | Known kf ->
        match kf with
        | BYR v -> sprintf "Birth Year of %s" v
        | IYR v -> sprintf "Issue Year of %s" v
        | EYR v -> sprintf "Expiration Year of %s" v
        | HGT v -> sprintf "Height of %s" v
        | HCL v -> sprintf "Hair Color of %s" v
        | ECL v -> sprintf "Eye Color of %s" v
        | PID v -> sprintf "Passport ID of %s" v
        | CID v -> sprintf "Country ID of %s" v
    | Unknown (fn, fv) -> sprintf "Unknown Field(%s:%s)" fn fv

let mkKnownByr = BYR >> Known
let mkKnownIyr = IYR >> Known
let mkKnownEyr = EYR >> Known
let mkKnownHgt = HGT >> Known
let mkKnownHcl = HCL >> Known
let mkKnownEcl = ECL >> Known
let mkKnownPid = PID >> Known
let mkKnownCid = CID >> Known

/// A passport is a list of fields, built from text wherein
/// the fields are delimited by spaces or newlines.
type Passport = PassportField list * bool

let isKnown pf =
    match pf with
    | Known _ -> true
    | Unknown _ -> false

let isRequiredField kf =
    match kf with
    | CID _ -> false
    | _ -> true

let isPassportValid passport =
    let count =
        fst passport
        |> List.fold (fun c field ->
            match field with
            | Known f -> if isRequiredField f then c + 1 else c
            | Unknown _ -> c) 0

    (fst passport, count = 7)

let mkEmptyPassport (): Passport = ([], false)

/// A batch file is a list of passports delimited by blank lines - \n\n
type PassportBatchFile = Passport list

let mkEmptyBatchFile (): PassportBatchFile = []

let buildToField fieldName =
    match fieldName with
    | "byr" -> mkKnownByr
    | "iyr" -> mkKnownIyr
    | "eyr" -> mkKnownEyr
    | "hgt" -> mkKnownHgt
    | "hcl" -> mkKnownHcl
    | "ecl" -> mkKnownEcl
    | "pid" -> mkKnownPid
    | "cid" -> mkKnownCid
    | _ -> (fun fv -> Unknown(fieldName, fv))

let toField (fn, fv) = buildToField fn fv

let splitByColon (s: string) =
    let parts = s.Split ':'
    (parts.[0], parts.[1])

let partToField = splitByColon >> toField

let partsToFields = List.map partToField

let splitBySpace (s: string) = s.Split(' ') |> Array.toList

let isBlank (str: string) = str.Length = 0 || str.Trim().Length = 0

let processDataLine = splitBySpace >> partsToFields >> Some

let processLine line =
    if isBlank line then None else processDataLine line

let addFields newFields currPassport = List.append newFields (fst currPassport)

let addToPassport newFields currPassport =
    (addFields newFields currPassport, snd currPassport)

let processLines lineSeq =
    let mutable currPassport = mkEmptyPassport ()
    let startingBatchFile = mkEmptyBatchFile ()
    lineSeq
    |> Seq.fold (fun accum next ->
        match processLine next with
        | Some fields ->
            currPassport <- addToPassport fields currPassport
            accum
        | None ->
            let updatedPassports = (isPassportValid currPassport) :: accum
            currPassport <- mkEmptyPassport ()
            updatedPassports) startingBatchFile
