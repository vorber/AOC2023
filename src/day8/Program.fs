open System.Text.RegularExpressions

let lineRx = Regex(@"^(?<name>\w+) = \((?<left>\w+), (?<right>\w+)\)$", RegexOptions.Compiled)

let input = AOC.Inputs.load "2023" "8" |> Async.RunSynchronously

type Node = {Name: string; Left: string; Right: string}
type Dir = Left | Right
let dir c = 
    match c with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "unexpected input"

let applyBoth f g x = (f x, g x)

let parseInput = 
    let g (m:Match) (group:string) = m.Groups[group].Value
    let parsePath = Seq.head >> Seq.map dir
    let parseNode = lineRx.Match >> fun m -> {Name = g m "name"; Left = g m "left"; Right = g m "right"}
    let parseNodes = Seq.skip 2 >> Seq.map parseNode >> Seq.map (fun n -> (n.Name, n)) >> Map
    applyBoth parsePath parseNodes

let (path, nodes) = parseInput input

let runUntil condition (path, start) = 
    let move node dir = match dir with | Left -> nodes[node.Left] | Right -> nodes[node.Right]
    (start, path) ||> Seq.scan move |> (Seq.takeWhile (condition >> not)) |> Seq.length |> int64

let rec repeat xs =  seq { yield! xs; yield! repeat xs}
let p1 = ((repeat path), nodes["AAA"]) |> runUntil ((=) nodes["ZZZ"])
printfn "P1: %i" p1

let isStart (name:string) _ = name[2] = 'A'
let isTerminal node = node.Name[2] = 'Z'

let startingNodes = Map.filter isStart >> Map.values

let tupleWith x y = (x, y)

let cycleLen = (tupleWith (repeat path)) >> runUntil isTerminal

let rec gcd a b = match a, b with
                    | (x, 0L) -> x
                    | (0L, y) -> y
                    | (x, y) -> gcd y (x % y)
let lcm a b = a * b / (gcd a b)

let part2 = startingNodes >> Seq.map cycleLen >> Seq.reduce lcm 

printfn "P2: %i" (part2 nodes)
