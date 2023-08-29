// split an iterable by a function across multiple elements
// https://stackoverflow.com/a/6737659/9348376
let splitBy f input =
  let i = ref 0
  input
  |> Seq.map(fun x ->
    // if some predicate matches
    if f x then incr i
    !i, x)
  |> Seq.groupBy fst
  |> Seq.map(fun (_, v) -> Seq.map snd v)

// converts a line into a map of key:value passport metadata
let parseLine (line: string) =
  line.Split ' '
  |> Array.map(fun token -> token.Split ':')
  |> Array.map(fun tokArr -> (tokArr.[0], tokArr.[1]))
  |> Map.ofArray

let parseFile (filename: string) =
  System.IO.File.ReadAllLines filename
  |> Seq.ofArray
  // split into individual passport data lines, split by empty lines
  |> splitBy ((=) "")
  // join multiple passport data lines into a string, then into a map
  |> Seq.map(fun pRawList -> (pRawList |> String.concat " ").Trim(' ') |> parseLine)
  |> Seq.toList

let partOne(pmap: Map<string,string>) =
  match Map.count(pmap) with
  | 8 -> true
  | 7 -> not (pmap.ContainsKey("cid"))
  | _ -> false

// returns True if 'value' can be converted to an integer,
// and its int value is between min and max
let between(value: string, min: int, max: int) =
  match System.Int32.TryParse(value) with
  | true, ivalue -> ivalue >= min && ivalue <= max
  | _ -> false

let validHeight(value: string) =
  let vlen: int = value.Length
  let vnum: string = value.Substring(0, vlen - 2)
  match value.Substring(vlen - 2, 2) with
  | "cm" -> between(vnum, 150, 193)
  | "in" -> between(vnum, 59, 76)
  | _ -> false

let validHairs = "1234567890abcdef" |> Seq.map string |> Set.ofSeq
let validHair(value: string) =
  value.Length = 7 && validHairs |> Set.isSubset(value.[1..6] |> Seq.map string |> Set.ofSeq)

let validEyes = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> Set.ofList
let validEye(value: string) =
  validEyes |> Set.contains(value)

let partTwoRule(key: string, value: string) =
  match key with
  | "byr" -> between(value, 1920, 2002)
  | "iyr" -> between(value, 2010, 2020)
  | "eyr" -> between(value, 2020, 2030)
  | "hgt" -> validHeight(value)
  | "hcl" -> validHair(value)
  | "ecl" -> validEye(value)
  | "pid" -> value.Length = 9 && between(value, 0, 999999999)
  | _ -> true

let partTwo(pmap: Map<string,string>) =
  partOne(pmap) &&
  pmap |> Map.toSeq |> Seq.forall(fun (key, value) -> partTwoRule(key, value))

let part filterf (prob: int) (lst: Map<string, string> list) =
  lst
  |> List.filter(fun m -> filterf(m))
  |> List.length
  |> sprintf "Part %d: %d" prob

[<EntryPoint>]
let main args =
  let passports = parseFile args.[0]
  [ partOne; partTwo ]
  |> List.mapi ( fun i func -> passports |> part func (i + 1))
  |> String.concat "\n"
  |> printfn "%s"
  0
