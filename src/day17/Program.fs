open AOC.Misc

type Direction = U | D | L | R
let rev d = match d with | U -> D | D -> U | L -> R | R -> L
type Vertex = {Pos: int*int; Dir: Direction; StepsTaken: int}

let parse = 
    let toInt c = (int c) - (int '0') 
    let makeGrid = Seq.map ((Seq.map toInt) >> Seq.toArray) >> Seq.toArray
    let size = applyBoth (Seq.head >> String.length) Seq.length
    applyBoth makeGrid size

let input = AOC.Inputs.load "2023" "17" |> Async.RunSynchronously
let (grid, size) = input |> parse
printfn "Size: %A" size

let step dir (x,y) =
    match dir with
    | U -> (x, y-1)
    | D -> (x, y+1)
    | L -> (x-1, y)
    | R -> (x+1, y)
let move v dir = {Dir = dir; Pos = step dir v.Pos; StepsTaken = if dir = v.Dir then v.StepsTaken + 1  else 1}

let inside (width, height) v = (fst v.Pos >= 0) && (fst v.Pos < width) && (snd v.Pos>=0) && (snd v.Pos < height) 

let validate sz v = List.map (move v) >> List.filter (inside sz) 

let neighbours_p1 sz v = 
    [U;D;L;R] 
    |> List.filter (fun dir -> dir <> rev v.Dir && (v.StepsTaken < 3 || v.Dir <> dir))
    |> validate sz v

let neighbours_p2 sz v = 
    let possibleDirections = 
        if v.StepsTaken > 0 && v.StepsTaken < 4 then [v.Dir]
        else [U;D;L;R] |> List.filter (fun dir -> dir <> rev v.Dir && (dir <> v.Dir || v.StepsTaken < 10))
    possibleDirections 
    |> validate sz v

let dijkstra (g:int array array) sz nfun start stepsLeft donefun = 
    let neighbours = nfun sz
    let loss (x,y) = g[y][x]
    let pop vs = 
        let v = vs |> Set.minElement
        (v, vs.Remove v)
    let rec loop queue visited = //TODO: refactor to Seq.unfold
        let (l, v), q' = pop queue
        if donefun v then (l, v)
        else
            let queue', visited' = 
                neighbours v
                        |> List.fold 
                            (fun (qq, vv) v' ->
                                let newLoss = l + loss v'.Pos
                                match vv |> Map.tryFind v' with
                                | None -> 
                                    (qq |> Set.add (newLoss, v'), vv |> Map.add v' (newLoss, v))
                                | Some (knownLoss, _) ->
                                    if (knownLoss <= newLoss) then (qq, vv)
                                    else (qq |> Set.add (newLoss, v'), vv |> Map.add v' (newLoss, v))
                            ) (q', visited)
            loop queue' visited' 

    loop (Set.singleton (0, {Dir = R; Pos=start; StepsTaken=stepsLeft})) Map.empty

let reachedFinalP1 v = (fst v.Pos) = (fst size)-1 && (snd v.Pos) = (snd size)-1
let reachedFinalP2 v = v.StepsTaken >=4 && reachedFinalP1 v

let part1 (grid, size) = dijkstra grid size neighbours_p1 (0,0) 3 reachedFinalP1
let part2 (grid, size) = dijkstra grid size neighbours_p2 (0,0) 0 reachedFinalP2
let (r1, r2) = input |> parse |> applyBoth part1 part2
printfn "%A" r1
printfn "%A" r2
