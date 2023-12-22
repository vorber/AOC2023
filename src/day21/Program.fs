open AOC.Misc

let parseInput = 
    let makeGrid = Seq.map (Seq.toArray) >> Seq.toArray
    let size = applyBoth (Seq.head >> String.length) Seq.length
    applyBoth makeGrid size

let walk grid start =
    let step positions =
        let stepFrom (x, y) = 
            let validStep (x, y) = grid (x, y) <> '#'
            [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] |> List.filter validStep |> Seq.ofList
        let p' = positions |> Set.toSeq |> Seq.collect stepFrom |> Set.ofSeq
        Some (p', p')
    Seq.unfold step (Set.singleton start)

let steps s = Seq.skip (s - 1) >> Seq.head

let p1 (grid:char [][], (w, h)) = walk (fun (x,y) -> grid[y][x]) (w/2, h/2) |> steps 64 |> Set.count
let p2 (grid:char [][], (w, h)) =
    let wrap a b = if a < 0 then (b + (a % b)) % b else a % b
    let steps = walk (fun (x,y) -> grid[wrap y h][wrap x w]) (w/2, h/2)
    let crossings = Seq.mapi (fun i e -> if i % w = (w/2 - 1) then Some e else None) >> Seq.choose id
    let c = steps |> crossings |> Seq.take 3 |> Seq.map (Set.count >> int64) |> Seq.toArray
    let n = 26501365L / int64 w
    c[0] + n * (c[1]-c[0]) + (n * (n-1L) / 2L) * ((c[2]-c[1]) - (c[1]-c[0]))

let (r1, r2) = AOC.Inputs.load "2023" "21" |> Async.RunSynchronously |> parseInput |> applyBoth p1 p2
printfn "%A" r1
printfn "%A" r2

