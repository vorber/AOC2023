open AOC.Misc

type Label = Label of int
type Brick = 
    { Label: Label; x1:int; y1:int; z1:int; x2:int; y2:int; z2:int; }
    member this.XY = Seq.allPairs [this.x1..this.x2] [this.y1..this.y2]
    member this.Height = this.z2 - this.z1
    static member make i ((x1,y1,z1), (x2,y2,z2)) = { Label = Label i; x1=x1; y1=y1; z1=z1; x2=x2; y2=y2; z2=z2; }
    static member parse i s = 
        let coords = (splitOn "~") >> tuple >> tmap (splitOn "," >> Array.map int >> triple)
        (i, coords s) ||> Brick.make

let stack =
    let drop (stack, supports) (brick: Brick) =
        let dropZ = Seq.map (flip Map.tryFind stack >> Option.map fst >> Option.defaultValue 0) >> Seq.max >> (+) 1
        let z = dropZ brick.XY
        let place = fun s p -> s |> Map.add p (z + brick.Height, brick.Label)
        let stack' = Seq.fold place stack
        let supportedBy = Seq.choose (flip Map.tryFind stack) >> Seq.filter (fst >> (=) (z-1)) >> Seq.map snd >> Set.ofSeq
        let supports' = supportedBy >> flip (Map.add brick.Label) supports
        brick.XY |> applyBoth stack' supports'
    Seq.fold drop (Map.empty, Map.empty)

let supports = Seq.mapi Brick.parse >> Seq.sortBy (_.z1) >> stack >> snd
let singles = Map.values >> Seq.choose Seq.tryExactlyOne >> Set.ofSeq

let part1 = applyBoth (singles >> Set.count) Seq.length >> fun (a, b) -> b - a

let part2 supports =
    let supportsOf l = supports |> Map.tryFind l |> Option.defaultValue Set.empty
    let rec willFallWithout ls =
        let stillStanding = Map.keys >> Set.ofSeq >> flip (-) ls
        let nextToFall = stillStanding >> Set.filter (fun l -> Set.isNonEmptySubset (supportsOf l) ls) >> Set.ofSeq
        match nextToFall supports with
        | s when Set.isEmpty s -> (Set.count ls) - 1
        | s -> willFallWithout (ls + s) 
    supports |> singles |> Seq.sumBy (Set.singleton >> willFallWithout)

let (r1, r2) = AOC.Inputs.load "2023" "22" |> Async.RunSynchronously |> supports |> applyBoth part1 part2 

printfn "part1: %A" r1
printfn "part2: %A" r2
