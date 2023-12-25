namespace AOC
open AOC.Misc
module Graph = 

    type Graph<'a> when 'a: comparison = 
        { 
            Vertices: 'a Set;
            OutgoingEdges:Map<'a, Map<'a, int>>;
            IncomingEdges: Map<'a, Map<'a, int>> 
        }

        member private this.connections selector v = 
            match (this |> selector |> Map.tryFind v) with
            | Some neighbours -> neighbours |> Map.keys |> Set.ofSeq
            | None -> Set.empty
        member private this.outgoingConnections = this.connections (_.OutgoingEdges)
        member private this.incomingConnections = this.connections (_.IncomingEdges)

        member this.topologicalSort from = 
            let rec sort (sorted: 'a list) (visited: 'a Set) (stack:'a Set) = 
                match stack with
                | Set.Empty -> sorted
                | Set.NonEmpty s -> 
                    let v = s |> Seq.head
                    let sorted' = v::sorted
                    let visited' = visited |> Set.add v
                    let neighbours = this.outgoingConnections v
                    let allIncomingConnectionsAreFrom s v = Set.isSubset (this.incomingConnections v |> Set.ofSeq) s
                    let next = (neighbours-visited) |> Set.filter (allIncomingConnectionsAreFrom visited') 
                    sort sorted' visited' (next + (stack |> Set.remove v))
            let result = sort [] Set.empty (Set.singleton from)
            result |> List.rev

        member this.longestAcyclicPathFrom start finish =
            let sorted = this.topologicalSort start
            let updateDistances (distances:Map<'a, int>) v =
                let updatedDistance u ds = 
                    let d' = distances[v] + ds
                    match distances |> Map.tryFind u with
                    | None -> d'
                    | Some d -> max d d'
                let ds' = this.OutgoingEdges[v] |> Map.map updatedDistance
                ds' |> Map.fold (fun m k v -> m |> Map.add k v) distances
            let distances = sorted |> List.fold updateDistances ([(start, 0)] |> Map)
            distances[finish]

        member this.bfs from (edgeFilter: 'a -> 'a -> bool) =
            let rec bfs (precedence: Map<'a,'a>) (visited: 'a Set) (queue: 'a list) =
                match queue with
                | [] -> precedence, visited
                | v::t -> 
                    let neighbours = this.outgoingConnections v |> Seq.filter (edgeFilter v) |> Set.ofSeq
                    let newNeighbours = neighbours-visited
                    let precedence' = newNeighbours |> Set.fold (fun m n -> m |> Map.add n v) precedence
                    let visited' = visited + newNeighbours
                    let queue' = t @ (newNeighbours |> Set.toList)
                    bfs precedence' visited' queue'
            bfs Map.empty (Set.singleton from) [from]
        
        member this.maxFlow source sink =
            let currentFlow v1 v2 = 
                let direct = Map.tryFind2 v1 v2
                let reverse = Map.tryFind2 v2 v1 >> Option.map (fun x -> -x)
                applyBoth direct reverse >> uncurry Option.orElse >> Option.defaultValue 0
            let nextPath flow =
                let edgeFilter v1 v2 = flow |> (currentFlow v1 v2) < 1
                let precedence, visited = this.bfs source edgeFilter
                let rec path (p: 'a list) v =
                    match precedence |> Map.tryFind v with
                    | Some u -> path (u::p) u
                    | None -> p
                if visited.Contains sink then Some (path [sink] sink) else None
            let rec updateFlow flow path =
                match path with
                | [] | [_] -> flow
                | v1::v2::t -> 
                    let flow' = 
                        let f = flow |> currentFlow v1 v2
                        flow |> Map.add v1 (flow |> Map.tryFind v1 |> Option.defaultValue Map.empty |> Map.add v2 (f+1))
                    updateFlow flow' (v2::t)
            let rec maxFlow flow current =
                match nextPath flow with
                | Some path -> 
                    let updatedFlow = updateFlow flow path 
                    maxFlow updatedFlow (current+1)
                | None -> flow, current
            maxFlow (Map [(source, Map.empty)]) 0

        member this.print () =
            printf "Vertices: "
            printf "%A" this.Vertices
            printfn ""
            printfn "Edges (Outgoing):"
            this.OutgoingEdges |> Map.iter (printfn "%A %A")
            printfn "Edges (Incoming):"
            this.IncomingEdges |> Map.iter (printfn "%A %A")

    let empty = { Vertices = Set.empty; OutgoingEdges = Map.empty ; IncomingEdges = Map.empty }
    let addVertex v this = { 
        this with 
            Vertices = Set.add v this.Vertices; 
            OutgoingEdges = this.OutgoingEdges |> Map.tryFind v |> Option.map (Const this.OutgoingEdges) |> Option.defaultValue (Map.add v Map.empty this.OutgoingEdges)
            IncomingEdges = this.IncomingEdges |> Map.tryFind v |> Option.map (Const this.IncomingEdges) |> Option.defaultValue (Map.add v Map.empty this.IncomingEdges)
    }
    let fromLabels labels = labels |> Seq.fold (fun g l -> addVertex l g) empty
    let addEdge v1 v2 d this = { 
        this with 
            OutgoingEdges = Map.add v1 (Map.add v2 d this.OutgoingEdges[v1]) this.OutgoingEdges 
            IncomingEdges = Map.add v2 (Map.add v1 d this.IncomingEdges[v2]) this.IncomingEdges
    }
    let addEdges this (from, vds) = vds |> Seq.fold (fun g (v, d) -> addEdge from v d g) this

