type Card = Card of char

let cardOrder (Card c) = "AKQJT98765432*" |> Seq.rev |> Seq.mapi (fun i c -> (c, i+1)) |> Map |> Map.find c

let compareCards c1 c2 = compare (cardOrder c1) (cardOrder c2)

type Hand = Hand of Card seq

let handTypeOrder cards =
    let cnt = cards |> Seq.filter (fun c -> c <> Card '*') |> Seq.countBy id |> Seq.map snd |> Seq.sortDescending //|> List.ofSeq
    match cnt |> Seq.length, cnt |> Seq.sum with
    | (0, _) | (1, _)   -> 6
    | (2, x)            -> cnt |> Seq.head |> (+) (6-x)
    | (3, x)            -> cnt |> Seq.head |> (+) (5-x)
    | (4, _)            -> 1
    | _                 -> 0

let compareHands (Hand h1) (Hand h2) =
    let c = compare (handTypeOrder h1) (handTypeOrder h2)
    if c <> 0 then c else
       (h1, h2) ||> Seq.compareWith compareCards

let input = AOC.Inputs.load "2023" "7" |> Async.RunSynchronously

type Round = {Hand:Hand; Bid:int}
let compareRounds r1 r2 = compareHands r1.Hand r2.Hand

let parseLine (line: string) =
    let parts = line.Split(" ") |> Array.map (fun s -> s.Trim())
    let hand = parts[0] |> Seq.map Card |> Hand
    {Hand=hand; Bid=int parts[1]}

let score lines = 
    lines
    |> Seq.map parseLine
    |> Seq.sortWith compareRounds
    |> Seq.mapi (fun i r -> r.Bid * (i+1))
    |> Seq.sum

let resultp1 = score input
let resultp2 = score (input |> Seq.map (fun line -> line.Replace("J", "*")))

printfn "Part 1: %i" resultp1
printfn "Part 2: %i" resultp2
