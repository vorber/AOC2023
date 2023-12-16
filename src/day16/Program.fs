open AOC.Misc
let flip = flipLastArgs

type Direction = U | D | L | R
let move ((x,y), dir) =
    match dir with
    | U -> ((x, y-1), dir)
    | D -> ((x, y+1), dir)
    | L -> ((x-1, y), dir)
    | R -> ((x+1, y), dir)

let interact (m:Map<int*int, char>) (pos, dir) =
    let changeDirection c = 
        match c, dir with
        | '.', d -> [d;]
        | '|', d when d = U || d = D -> [d;]
        | '|', d when d = L || d = R -> [U;D;]
        | '-', d when d = U || d = D -> [R;L;]
        | '-', d when d = L || d = R -> [d;]
        | '\\', U | '/', D -> [L;]
        | '\\', D | '/', U -> [R;]
        | '\\', L | '/', R -> [U;]
        | '\\', R | '/', L -> [D;]
        | _ -> failwith "unexpected"
    match m.TryFind(pos) with
    | None -> []
    | Some c -> changeDirection c
    |> List.map (fun d -> (pos, d))
    |> Set.ofList

type NodeSet = Set<(int*int)*Direction>
let traceBFS grid (start, direction) =
    let rec trace visited ps =
        let next = Set.map (interact grid) >> Set.unionMany >> Set.map move >> Set.filter (fst >> grid.ContainsKey)
        match (next ps) - visited with
        | s when Set.isEmpty s -> visited
        | s -> trace (visited + s) s
    trace (Set.empty.Add (start, direction)) (Set.empty.Add (start, direction))

let energizedCount grid = traceBFS grid >> Set.map fst >> Set.count

let parseInput = Seq.mapi (fun y l -> l |> Seq.mapi (fun x c -> ((x, y), c))) >> Seq.concat >> Map

let solveP1 = parseInput >> energizedCount
let part1 i = i |> solveP1 <| ((0,0),R)
let part2 i = 
    let grid = parseInput i
    let h = i |> Seq.length
    let w = i |> Seq.head |> String.length
    let top = [1..w-1] |> List.map (fun x -> ((x,0), D))
    let bottom = [1..w-1] |> List.map (fun x -> ((x,w-1), U))
    let left = [1..h-1] |> List.map (fun x -> ((x,0), R))
    let right = [1..h-1] |> List.map (fun x -> ((x,h-1), L))
    let best = [top; bottom; left; right] |> List.map (List.map (energizedCount grid) >> List.max) |> List.max
    best

let (r1, r2) = AOC.Inputs.load "2023" "16" |> Async.RunSynchronously |> applyBoth part1 part2
printfn "%A" r1
printfn "%A" r2

