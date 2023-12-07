type Card = Card of char

let cardOrder (Card c) = "AKQJT98765432*" |> Seq.rev |> Seq.mapi (fun i c -> (c, i+1)) |> Map |> Map.find c

let compareCards c1 c2 = compare (cardOrder c1) (cardOrder c2)
let isWildCard = (Card '*') |> (=)
type Hand = Hand of Card seq
let parseHand: (string->Hand) = Seq.map Card >> Hand

let handOrder cards =
    let cnt = cards |> Seq.filter (isWildCard >> not) |> Seq.countBy id |> Seq.map snd |> Seq.sortDescending //|> List.ofSeq
    let mostFrequent = cnt |> Seq.tryHead |> Option.defaultValue 0
    let wildCnt = cnt |> Seq.sum |> (-) 5
    match mostFrequent + wildCnt, cnt |> Seq.length with
    | (5, _) -> 6
    | (4, 2) -> 5
    | (3, 2) -> 4
    | (3, 3) -> 3
    | (2, 3) -> 2
    | (2, 4) -> 1
    | _      -> 0

let compareHands (Hand a) (Hand b) =
    match compare (handOrder a) (handOrder b) with
    | 0 -> (a, b) ||> Seq.compareWith compareCards
    | c -> c

let input = AOC.Inputs.load "2023" "7" |> Async.RunSynchronously

type Round = {Hand:Hand; Bid:int}
let compareRounds a b = compareHands a.Hand b.Hand

let parseLine (line: string) =
    let parts = line.Split(" ") |> Array.map (fun s -> s.Trim())
    {Hand=parseHand parts[0]; Bid=int parts[1]}

let score: (string seq -> int) = 
    Seq.map parseLine >> Seq.sortWith compareRounds >> Seq.mapi (fun i r -> r.Bid * (i+1)) >> Seq.sum

let resultp1 = score input
let resultp2 = input |> Seq.map (fun line -> line.Replace("J", "*")) |> score

printfn "Part 1: %i" resultp1
printfn "Part 2: %i" resultp2
