open AOC
open AOC.Misc

type VertexLabel = Label of string
type G = Graph.Graph<VertexLabel>
let parse =
    let addEdges g (l, ls) = 
        (g |> Graph.addVertex l, ls) 
        ||> Array.fold (fun g l' -> g |> Graph.addVertex l' |> Graph.addEdge l l' 1 |> Graph.addEdge l' l 1)
    let parseLine = (splitOn ": ") >> tuple >> tmap2 Label (splitOn " " >> Array.map Label)
    Seq.fold (fun g l -> parseLine l |> addEdges g) Graph.empty

let part1 (graph:G) = 
    let findSuitableFlow (graph: G) = 
        let maxFlow = uncurry graph.maxFlow
        let distinctPairs = dup >> uncurry Seq.allPairs >> Seq.filter (uncurry (<>))
        graph.Vertices 
        |> distinctPairs
        |> Seq.map (applyBoth id maxFlow) 
        |> Seq.find (snd >> snd >> (=) 3)
        |> tmap2 fst fst
    let edgeFilter f v1 v2 = f |> Map.tryFind2 v1 v2 |> Option.defaultValue 0 |> (>) 1
    let src, filter = graph |> findSuitableFlow |> tmap2 id edgeFilter
    let _, visited = graph.bfs src filter
    (Set.count visited, Set.count (graph.Vertices - visited)) ||> (*)

let r1 = AOC.Inputs.load "2023" "25" |> Async.RunSynchronously |> parse |> part1 
printfn "Part 1: %A" r1
