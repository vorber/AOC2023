open System
let input =
    AOC.Inputs.load "2023" "4" |> Async.RunSynchronously
//    [|
//        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
//        "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19";
//        "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
//        "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83";
//        "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36";
//        "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
//    |] :> seq<string>

let parseLine (line:string) = 
    let withoutPrefix = line.Split(": ")[1]
    let split = withoutPrefix.Split(" | ")
    let toInt (s:string) = int(s.Trim())
    let toNumbers (s:string) =
        s.Split(" ")
        |> Seq.ofArray
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.map toInt
    let winning = toNumbers split[0] |> Set.ofSeq
    let actual = toNumbers split[1]
    (winning, actual)

let countPoints line =
    let (winning, actual) = parseLine line
    actual 
    |> Seq.filter (fun x -> Set.contains x winning)
    |> Seq.length

//part 1 specific
let part1Points line =
    let winCount = countPoints line
    if winCount > 0 then 1 <<< (winCount-1) else 0

let totalPoints = 
    input
    |> Seq.map part1Points
    |> Seq.sum

printfn "Total points: %i" totalPoints
//part 2
let points =
    input 
    |> Seq.map countPoints
    |> List.ofSeq

type bonus = {Range: int; Increment: int}
let totalCards = 
    let decRange (l:bonus list) = 
        l 
        |> List.map (fun x -> {Range = x.Range - 1; Increment = x.Increment})
        |> List.filter (fun x -> x.Range>0)

    let rec count acc (bacc:bonus list) (pts:int list) =
        let bonusCount = bacc |> List.sumBy (fun x -> x.Increment)
        let increment = 1 + bonusCount
        let nextbacc pts = if pts = 0 then decRange bacc else {Range = pts; Increment = increment}::(decRange bacc)
        match pts with
        | [] -> acc
        | x::xs -> count (increment+acc) (nextbacc x) xs
    count 0 [] points

printfn "Total scratchcards: %i" totalCards
