open System.Text.RegularExpressions

type Node = {Name: string; Left: string; Right: string}
type Dir = Left | Right
let dir c = 
    match c with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "unexpected input"

let applyBoth f g x = (f x, g x) //TODO: move to shared lib?

let parseInput = 
    let lineRx = Regex(@"^(?<name>\w+) = \((?<left>\w+), (?<right>\w+)\)$", RegexOptions.Compiled)
    let g (m:Match) (group:string) = m.Groups[group].Value
    let parsePath = Seq.head >> Seq.map dir
    let parseNode = lineRx.Match >> fun m -> {Name = g m "name"; Left = g m "left"; Right = g m "right"}
    let parseNodes = Seq.skip 2 >> Seq.map parseNode >> Seq.map (fun n -> (n.Name, n)) >> Map
    applyBoth parsePath parseNodes

let runUntil condition nodes start = 
    let rec repeat xs =  seq { yield! xs; yield! repeat xs} //TODO: move to shared lib?
    let move node dir = match dir with | Left -> nodes |> Map.find node.Left | Right -> nodes |> Map.find node.Right
    repeat >> Seq.scan move start >> Seq.takeWhile (condition >> not) >> Seq.length >> int64

let part1 (path, nodes) =
    let isTerminal node = node.Name = "ZZZ"
    runUntil isTerminal nodes nodes["AAA"] path

let rec gcd a b = match a, b with
                    | (x, 0L) -> x
                    | (0L, y) -> y
                    | (x, y) -> gcd y (x % y)
let lcm a b = a * b / (gcd a b)

let part2 (path, nodes) =  
    let isStart (name:string) _ = name.EndsWith 'A'
    let isTerminal node = node.Name.EndsWith 'Z'
    let flipLastArg f a b = f b a //TODO: move to shared lib?
    let runParallel = flipLastArg (runUntil isTerminal nodes) path
    let result = Map.filter isStart >> Map.values >> Seq.map runParallel >> Seq.reduce lcm
    nodes |> result

let r1,r2 = 
    AOC.Inputs.load "2023" "8" 
    |> Async.RunSynchronously 
    |> parseInput
    |> applyBoth part1 part2
printfn "P1: %i" r1 
printfn "P2: %i" r2
