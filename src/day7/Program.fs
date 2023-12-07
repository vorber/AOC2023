type Card = Card of char

let ordered = "AKQJT98765432*" 
let orderMap = (ordered, (ordered |> String.length |> Seq.init) (fun x -> x+1) |> Seq.rev) ||> Seq.zip |> Map.ofSeq

let cardOrder (card:Card) = 
    let (Card c) = card
    orderMap[c]

let compareCards c1 c2 = compare (cardOrder c1) (cardOrder c2)

type Hand = Hand of Card seq

let (|FiveOfAKind|FourOfAKind|FullHouse|ThreeOfAKind|TwoPairs|OnePair|HighCard|) hand =
    let (Hand cards) = hand
    let wildcards = cards |> Seq.filter (fun c -> c = Card '*') |> Seq.length
    let uniqueCount = cards |> Seq.countBy id |> Seq.map snd |> Seq.sortDescending |> List.ofSeq
    match (uniqueCount, wildcards) with
    | ([5], _) -> FiveOfAKind
    | ([4; 1], 1) -> FiveOfAKind
    | ([4; 1], 4) -> FiveOfAKind
    | ([4; 1], 0) -> FourOfAKind
    | ([3; 2], 0) -> FullHouse
    | ([3; 2], _) -> FiveOfAKind
    | ([3; 1; 1], 0) -> ThreeOfAKind
    | ([3; 1; 1], _) -> FourOfAKind
    | ([2; 2; 1], 0) -> TwoPairs
    | ([2; 2; 1], 1) -> FullHouse
    | ([2; 2; 1], 2) -> FourOfAKind
    | ([2; 1; 1; 1], 0) -> OnePair
    | ([2; 1; 1; 1], _) -> ThreeOfAKind
    | ([1; 1; 1; 1; 1], 1) -> OnePair
    | ([1; 1; 1; 1; 1], 0) -> HighCard
    | _ -> failwith "invalid hand"

let handTypeOrder hand =
    match hand with
    | HighCard -> 0
    | OnePair -> 1
    | TwoPairs -> 2
    | ThreeOfAKind -> 3
    | FullHouse -> 4
    | FourOfAKind -> 5
    | FiveOfAKind -> 6

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
