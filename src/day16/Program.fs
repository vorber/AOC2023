open AOC.Misc

let flip = flipLastArgs

type Direction = U | D | L | R
let move ((x,y), dir) =
    match dir with
    | U -> ((x, y-1), dir)
    | D -> ((x, y+1), dir)
    | L -> ((x-1, y), dir)
    | R -> ((x+1, y), dir)

let parseInput = 
    let makeGrid = Array.map (Array.ofSeq)
    let size = applyBoth (Seq.head >> String.length) Seq.length
    applyBoth makeGrid size

let (grid, size) = AOC.Inputs.load "2023" "16" |> Async.RunSynchronously |> parseInput
let inside (x,y) = (x >= 0) && (x < fst size) && (y>=0) && (y < snd size)

let interact ((x,y), dir) =
    let changeDirection c = 
        match c, dir with
        | '.', d -> Set.empty.Add(d) 
        | '|', d when d = U || d = D -> Set.empty.Add(d)
        | '|', d when d = L || d = R -> Set.empty.Add(U).Add(D)
        | '-', d when d = U || d = D -> Set.empty.Add(R).Add(L)
        | '-', d when d = L || d = R -> Set.empty.Add(d)
        | '\\', U | '/', D -> Set.empty.Add(L)
        | '\\', D | '/', U -> Set.empty.Add(R)
        | '\\', L | '/', R -> Set.empty.Add(U)
        | '\\', R | '/', L -> Set.empty.Add(D)
        | _ -> failwith "unexpected"
    if inside (x,y) then (grid[y][x]) |> changeDirection |> Set.map (fun d -> ((x,y), d)) else Set.empty

let energize (pos, dir) =
    Seq.unfold 
        (fun (visited, ps) -> 
            match ps with 
            | p when Set.isEmpty p -> None
            | p -> 
                let next = Set.map interact >> Set.unionMany >> Set.map move >> Set.filter (fst >> inside)
                let ps' = (next p) - visited
                let visited' = visited + ps'
                Some (visited', (visited', ps')))
        (Set.empty.Add (pos, dir), Set.empty.Add (pos, dir))
    |> Seq.last
    |> Set.map fst
    |> Set.count

let part1 = energize ((0,0),R)

printfn "%A" part1

let part2 = 
    let (w, h) = size
    let top = [|1..w-1|] |> Array.map (fun x -> ((x,0), D))
    let bottom = [|1..w-1|] |> Array.map (fun x -> ((x,w-1), U))
    let left = [|1..h-1|] |> Array.map (fun x -> ((x,0), R))
    let right = [|1..h-1|] |> Array.map (fun x -> ((x,h-1), L))
    let best = [|top; bottom; left; right|] |> Array.Parallel.map (Array.Parallel.map energize >> Array.max) |> Array.max
    best

printfn "%A" part2 

