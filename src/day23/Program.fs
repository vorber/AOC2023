open AOC
open AOC.Misc

let parse =
    let processLine line = Seq.mapi (fun i a -> ((i, line), a))
    let makeGrid = Seq.mapi processLine >> Seq.concat >> Map
    let size = applyBoth (Seq.head >> String.length) Seq.length
    applyBoth makeGrid size

type VertexLabel = Label of int * int
type G = Graph.Graph<VertexLabel>

let solve =
    let compressGrid ((grid, (width, height)), allowedMoves) =
        let adjacent =
            let move p = Direction.move p 1
            let canMoveTo (x, y) = x >= 0 && x < width && y >= 0 && y < height && Map.find (x,y) grid <> '#'
            applyBoth move allowedMoves >> uncurry Seq.map >> Seq.filter canMoveTo
        let isNode = 
            let notForest = snd >> (=) '#' >> not
            let notSlope = fst >> allowedMoves >> List.length >> (=) 1 >> not
            let notTrail = fst >> adjacent >> Seq.length >> (=) 2 >> not
            notForest <&> notSlope <&> notTrail

        let nodes = grid |> Map.filter (curry isNode) |> Map.keys |> Set.ofSeq
        let nodeNeighbours p =
            let rec walk steps visited pos =
                let step c = (c, visited) ||> Set.add |> walk (steps + 1)
                let next step = adjacent >> Seq.filter ((flip Set.contains visited) >> not) >> Seq.tryExactlyOne >> Option.bind step
                match pos with
                | n when Set.contains n nodes -> Some (n, steps)
                | c -> (step c, c) ||> next
            let startWalk = p |> Set.singleton |> walk 1
            p |> applyBoth Label (adjacent >> Seq.choose startWalk >> Seq.map (tmap2 Label id))
        let makeGraph = applyBoth (Seq.map Label >> Graph.fromLabels) (Seq.map nodeNeighbours) >> uncurry (Seq.fold Graph.addEdges)
        Label (1,0), Label (width - 2, height - 1), nodes |> makeGraph

    let rec dfs visited (best:int) (pos, dest, graph:G) =
        if pos = dest then best
        else
            let next = graph.OutgoingEdges[pos] |> Map.filter (fun v _ -> visited |> Set.contains v |> not) 
            match next with 
            | m when Map.isEmpty m -> 0
            | m -> m |> Map.map (fun v d -> dfs (visited |> Set.add v) (best + d) (v, dest, graph)) |> Map.values |> Seq.max

    compressGrid >> dfs Set.empty 0

let part1 input = 
    let allowedDirections p = 
        match input |> fst |> Map.tryFind p with
        | Some '>' -> [Right]
        | Some 'v' -> [Down]
        | Some '<' -> [Left]
        | Some '^' -> [Up]
        | _ -> Direction.all
    solve (input, allowedDirections)

let part2 = applyBoth id (Direction.all |> Const |> Const) >> solve

let (r1, r2) = AOC.Inputs.load "2023" "23" |> Async.RunSynchronously |> parse |> applyBoth part1 part2

printfn "%A" r1
printfn "%A" r2
