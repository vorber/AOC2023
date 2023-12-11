open AOC.Misc
let parseInput =
    let processLine line = Seq.mapi (fun i a -> ((i, line), a))
    let startPosInLine = Seq.tryFindIndex ((=) 'S')
    let makeGrid = Seq.mapi processLine >> Seq.concat >> Map
    let findStart =
        (Seq.mapi (fun i l -> (i, startPosInLine l))) 
        >> Seq.choose (fun (y,x) -> x |> Option.map (fun s -> (s, y)))
        >> Seq.head//TODO: simplify
    applyBoth makeGrid findStart 

type Direction = Up | Down | Left | Right
let inv dir = match dir with 
                | Up -> Down
                | Down -> Up
                | Left -> Right
                | Right -> Left

let move (x, y) dir = 
    match dir with
    | Up -> (x, y-1)
    | Down -> (x, y+1)
    | Left -> (x-1, y)
    | Right -> (x+1, y)

let flow grid start direction =
    let nextMoveGenerator (grid: Map<int*int, char>) (from, dir) = 
        let whereToNext from cell =
            match cell with
            | '|' -> if from = Up then Some Down elif from = Down then Some Up else None
            | '-' -> if from = Left then Some Right elif from = Right then Some Left else None
            | '7' -> if from = Left then Some Down elif from = Down then Some Left else None
            | 'F' -> if from = Right then Some Down elif from = Down then Some Right else None
            | 'L' -> if from = Up then Some Right elif from = Right then Some Up else None
            | 'J' -> if from = Up then Some Left elif from = Left then Some Up else None
            | _ -> None
        let next = move from dir
        let nextMove = whereToNext (inv dir) >> Option.map (fun d -> (next, (next, d)))
        (Map.tryFind next grid) |> Option.bind nextMove
    Seq.unfold (nextMoveGenerator grid) (start, direction) 

let path (grid, start) = 
    //this is somewhat cheating, since technically there can be a longer path that doesn't loop
    //also going through the loop twice, in both directions. TODO: improve
    let withoutStart = [Up; Down; Left; Right] |> List.map (flow grid start) |> List.maxBy Seq.length
    seq { yield start; yield! withoutStart; yield start; }

let half = flipLastArgs (/) 2

let area = Seq.pairwise >> Seq.fold (fun s ((x1, y1), (x2, y2)) -> s + x1*y2 - x2*y1 ) 0  >> abs >> half

let part1 = Seq.length >> half
let part2 path = 
    let (a, p1) = (applyBoth area part1) path
    a - p1 + 1 //magic :P
    
let solution = parseInput >> path >> applyBoth part1 part2
let (p1, p2) = AOC.Inputs.load "2023" "10" |> Async.RunSynchronously |> solution
printfn "part1: %i" p1 
printfn "part2: %i" p2
