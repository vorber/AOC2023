type Card = Card of char

let ordered = "AKQJT98765432*" |> Seq.rev |> Seq.mapi (fun i c -> (c, i+1)) |> Map

let cardOrder (card:Card) = 
    let (Card c) = card
    ordered[c]

let compareCards c1 c2 = compare (cardOrder c1) (cardOrder c2)

type Hand = Hand of Card seq

let handTypeOrder hand =
    let (Hand cards) = hand
    let withoutJokers = cards |> Seq.filter (fun c -> c <> Card '*')
    let counts = withoutJokers |> Seq.countBy id |> Seq.map snd |> Seq.sortDescending |> List.ofSeq
    match counts with
    | [5] | [4] | [3] | [2] | [1] | []  -> 6
    | [4; 1] | [3; 1] | [2; 1] | [1; 1] -> 5
    | [3; 2] | [2; 2]                   -> 4
    | [3; 1; 1] | [2; 1; 1] | [1; 1; 1] -> 3
    | [2; 2; 1]                         -> 2
    | [2; 1; 1; 1;] | [1; 1; 1; 1]      -> 1
    | _                                 -> 0

let compareHands (Hand h1) (Hand h2) =
    let c = compare (handTypeOrder (Hand h1)) (handTypeOrder (Hand h2))
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
