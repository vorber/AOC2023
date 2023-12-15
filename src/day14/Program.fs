open AOC.Misc

type Cell = Block | Rock | Empty
type TiltDir = North | East | South | West
type RotDir = Forward | Back

let parseCell c =
    match c with
    | 'O' -> Rock
    | '#' -> Block
    | '.' -> Empty
    | _ -> failwith "unexpected"

let parseInput = Seq.map (Seq.map parseCell >> List.ofSeq) >> List.ofSeq

let rotate tdir rdir =
    match rdir, tdir with
    | _, West -> id
    | _, East -> List.map List.rev
    | _, North -> List.transpose
    | Forward, South -> List.rev >> List.transpose
    | Back, South -> List.transpose >> List.rev

let weight = List.mapi (fun i a -> (i+1, a)) >> List.filter (snd >> ((=) Rock)) >> List.sumBy fst
let platformWeight = rotate South Forward >> List.sumBy weight

let tilt = (splitBefore ((=) Block)) >> List.map (List.sort) >> List.concat
let tiltPlatform dir = rotate dir Forward >> List.map tilt >> rotate dir Back

let part1 = tiltPlatform North >> platformWeight

let part2 = 
    let cycle = [North; West; South; East] |> List.map tiltPlatform |> List.fold (>>) id
    let findPeriod =
        let tp0 x = (0, x)
        let rec run cache (i, platform) =
            match cache |> Map.tryFind platform with
            | None -> run (Map.add platform i cache) (i+1, cycle platform)
            | Some start -> (start, i - start, cache)
        tp0 >> run Map.empty
    let finalPlatformState (ps, pl, cache) = Map.findKey (fun _ i -> i = ps + (1000000000 - ps) % pl) cache

    findPeriod >> finalPlatformState >> platformWeight

let (r1, r2) = AOC.Inputs.load "2023" "14" |> Async.RunSynchronously |> parseInput |> applyBoth part1 part2
printfn "P1: %A" r1
printfn "P2: %A" r2
