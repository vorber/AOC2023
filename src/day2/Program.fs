open System.Text.RegularExpressions
open AOC

type Set = {Red: int option; Blue: int option; Green: int option}
type Game = {Index: int; Sets: Set List }
let inputs = 
    (Inputs.load "2023" "2") |> Async.RunSynchronously
//let inputs =
//    [|
//        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
//        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
//        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
//        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
//        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
//    |]

let gameRx = Regex(@"^Game (?<index>\d+)", RegexOptions.Compiled)
let colorRx = Regex(@"(?<count>\d+) (?<color>blue|red|green)", RegexOptions.Compiled)

let parseGameIndex game = 
    let m = gameRx.Match(game)
    m.Groups["index"].Value |> int

let parseSingleSet (line:string) =
    let items = colorRx.Matches(line)
    let getCount color = 
        let matches = items |> Seq.filter (fun m -> m.Groups["color"].Value = color) |> Seq.toList
        match matches with
        | [] -> None
        | _ -> int matches.Head.Groups["count"].Value |> Some
    { Red = getCount "red"; Green = getCount "green"; Blue = getCount "blue" }

let parseSets (line:string) = line.Split("; ") |> Array.toList |> List.map parseSingleSet

let parseLine (line:string) = 
    let tokens = line.Split(':')
    let game = tokens[0]
    {Index = parseGameIndex game; Sets = parseSets tokens[1]}

let limits = 
    [
        "red", 12;
        "blue", 14;
        "green", 13;
    ] |> Map.ofList

let games = inputs |>Array.map parseLine

let isNoneOrWithinLimit limit value =
    match value with
    | Some v -> v <= limits[limit]
    | None -> true

let isValidSet s = 
    isNoneOrWithinLimit "red" s.Red && isNoneOrWithinLimit "blue" s.Blue && isNoneOrWithinLimit "green" s.Green

let isValid game = game.Sets |> List.forall isValidSet

let validGames = games |> Array.filter isValid

let sumOfValidGameIndexes = 
    validGames
    |> Array.map (fun g -> g.Index)
    |> Array.sum

printfn "Sum of valid game indexes: %i" sumOfValidGameIndexes

let minimumSet game =
    let maxFor f = game.Sets |> Seq.map f |> Seq.max
    {Red = maxFor (fun s -> s.Red); Green = maxFor (fun s -> s.Green); Blue = maxFor (fun s -> s.Blue)} 

let setPower s = 
    let valueOr1 o = match o with | Some x -> x | None -> 1
    (valueOr1 s.Red) * (valueOr1 s.Blue) * (valueOr1 s.Green)

games
    |> Array.map minimumSet
    |> Array.map setPower
    |> Array.sum
    |> printfn "%i"
