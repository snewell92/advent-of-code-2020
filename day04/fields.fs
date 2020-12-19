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

let isCid kf =
    match kf with
    | CID _ -> true
    | _ -> false

/// Fulfills part 1
let isRequiredField kf = not <| isCid kf

let isNum (s: string) =
    match System.Int32.TryParse s with
    | true, num -> Some num
    | _ -> None

let inRange lower upper num = num >= lower && num <= upper

let optIsSomeTrue opt =
    match opt with
    | Some b -> b
    | None -> false

let checkHeight (heightStr: string) =
    let endsWithCm = heightStr.EndsWith("cm")
    let endsWithIn = heightStr.EndsWith("in")
    if endsWithCm then
        heightStr.Substring(0, 3)
        |> isNum
        |> Option.map (inRange 150 193)
        |> optIsSomeTrue
    else if endsWithIn then
        heightStr.Substring(0, 2)
        |> isNum
        |> Option.map (inRange 59 76)
        |> optIsSomeTrue
    else
        false

let checkDateRange lower upper =
    isNum
    >> Option.map (inRange lower upper)
    >> optIsSomeTrue

let rec allValidHexDigits (chars: char list) =
    match chars with
    | '0' :: rest -> allValidHexDigits rest
    | '1' :: rest -> allValidHexDigits rest
    | '2' :: rest -> allValidHexDigits rest
    | '3' :: rest -> allValidHexDigits rest
    | '4' :: rest -> allValidHexDigits rest
    | '5' :: rest -> allValidHexDigits rest
    | '6' :: rest -> allValidHexDigits rest
    | '7' :: rest -> allValidHexDigits rest
    | '8' :: rest -> allValidHexDigits rest
    | '9' :: rest -> allValidHexDigits rest
    | 'a' :: rest -> allValidHexDigits rest
    | 'b' :: rest -> allValidHexDigits rest
    | 'c' :: rest -> allValidHexDigits rest
    | 'd' :: rest -> allValidHexDigits rest
    | 'e' :: rest -> allValidHexDigits rest
    | 'f' :: rest -> allValidHexDigits rest
    | [] -> true
    | _ -> false

let checkValidHexColor (hcl: string) =
    if hcl.[0] = '#' then
        hcl.Substring(1)
        |> List.ofSeq
        |> allValidHexDigits
    else
        false

let checkValidHairColor (ecl: string) =
    match ecl with
    | "amb" -> true
    | "blu" -> true
    | "brn" -> true
    | "gry" -> true
    | "grn" -> true
    | "hzl" -> true
    | "oth" -> true
    | _ -> false

let isStrLen len (str: string) = str.Length = len

let checkValidPid pidStr =
    if isStrLen 9 pidStr then
        pidStr
        |> isNum
        |> Option.map (fun _ -> true)
        |> optIsSomeTrue
    else
        false

/// Fulfills part 2
let isValidField kf =
    match kf with
    | CID _ -> true
    | BYR byrStr -> checkDateRange 1920 2002 byrStr
    | IYR iyrStr -> checkDateRange 2010 2020 iyrStr
    | EYR eyrStr -> checkDateRange 2020 2030 eyrStr
    | HGT hgtStr -> checkHeight hgtStr
    | HCL hclStr -> checkValidHexColor hclStr
    | ECL eclStr -> checkValidHairColor eclStr
    | PID pidStr -> checkValidPid pidStr

let isPassportValid passport =
    let (count, isValid, hasCid) =
        fst passport
        |> List.fold (fun (c, allValid, hasCid) field ->
            match field with
            | Known f -> (c + 1, allValid && isValidField f, hasCid || isCid f)
            | Unknown _ -> (c, allValid, hasCid)) (0, true, false)

    (fst passport,
     isValid
     && (count = 8 || (count = 7 && not hasCid)))

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
