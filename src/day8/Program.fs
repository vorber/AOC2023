open System.Text.RegularExpressions

let lineRx = Regex(@"^(?<name>\w+) = \((?<left>\w+), (?<right>\w+)\)$", RegexOptions.Compiled)
let testInput1 = seq {
    "RL";
    "";
    "AAA = (BBB, CCC)";
    "BBB = (DDD, EEE)";
    "CCC = (ZZZ, GGG)";
    "DDD = (DDD, DDD)";
    "EEE = (EEE, EEE)";
    "GGG = (GGG, GGG)";
    "ZZZ = (ZZZ, ZZZ)";
}

let testInput2 = seq {
    "LLR";
    "";
    "AAA = (BBB, BBB)";
    "BBB = (AAA, ZZZ)";
    "ZZZ = (ZZZ, ZZZ)";
}

let input = AOC.Inputs.load "2023" "8" |> Async.RunSynchronously

type Node = {Name: string; Left: string; Right: string}
type Dir = Left | Right
let dir c = 
    match c with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "unexpected input"


let applyBoth f g x = (f x, g x)

let parseInput : (seq<string>  -> seq<Dir>*Map<string, Node>) =
    let g (m:Match) (group:string) = m.Groups[group].Value
    let parsePath = Seq.head >> Seq.map dir
    let parseNode = lineRx.Match >> fun m -> {Name = g m "name"; Left = g m "left"; Right = g m "right"}
    let parseNodes = Seq.tail >> Seq.tail >> Seq.map parseNode >> Seq.map (fun n -> (n.Name, n)) >> Map
    applyBoth parsePath parseNodes

let (path, nodes) = parseInput input//testInput2//testInput1

let cycleWhile condition (path, start) = 
    let move node dir = match dir with | Left -> nodes[node.Left] | Right -> nodes[node.Right]
    (start, path) ||> Seq.scan move |> (Seq.takeWhile condition) |> Seq.length |> int64

let rec repeat xs =  seq { yield! xs; yield! repeat xs}
let p1 = ((repeat path), nodes["AAA"]) |> cycleWhile ((<>) nodes["ZZZ"])
printfn "P1: %i" p1

let isStart (name:string) _ = name[2] = 'A'
let isTerminal node = node.Name[2] = 'Z'

let startingNodes = Map.filter isStart >> Map.values

let tupleWith x y = (x, y)

let cycleLen = (tupleWith (repeat path)) >> cycleWhile (isTerminal >> not)

let rec gcd a b = 
    match a, b with
                    | (x, 0L) -> x
                    | (0L, y) -> y
                    | (x, y) -> gcd y (x % y)
let lcm a b = a * b / (gcd a b)

let part2: (Map<string,Node> -> int64) = startingNodes >> Seq.map cycleLen >> Seq.fold lcm 1L

printfn "P2: %i" (part2 nodes)
