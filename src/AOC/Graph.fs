namespace AOC
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
                | s when Set.isEmpty s -> sorted
                | s -> 
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
            OutgoingEdges = Map.add v Map.empty this.OutgoingEdges 
            IncomingEdges = Map.add v Map.empty this.IncomingEdges
    }
    let fromLabels labels = labels |> Seq.fold (fun g l -> addVertex l g) empty
    let addEdge v1 v2 d this = { 
        this with 
            OutgoingEdges = Map.add v1 (Map.add v2 d this.OutgoingEdges[v1]) this.OutgoingEdges 
            IncomingEdges = Map.add v2 (Map.add v1 d this.IncomingEdges[v2]) this.IncomingEdges
    }
    let addEdges this (from, vds) = vds |> Seq.fold (fun g (v, d) -> addEdge from v d g) this

