open AOC.Misc

let rec arrange = 
    let matches s g =
        let max = s |> List.takeWhile ((<>) '.') |> List.length
        if max < g then false 
        else match s |> List.skip g |> List.tryHead with
                | Some '#' -> false
                | _ -> true
    memoize 
        (fun (pat, grp) ->
            match pat, grp with
            | [], [] -> 1L
            | [], _ -> 0L
            | p, [] -> if p |> List.forall((<>) '#') then 1L else 0L
            | p::ps, g::gs -> 
                let ret = 
                    match p with
                    | '.' -> arrange (ps, g::gs)
                    | '?' -> (arrange (ps, g::gs)) + (arrange ('#'::ps, g::gs))
                    | '#' -> 
                        if matches (p::ps) g then arrange (ps |> safeSkip g |> List.ofSeq, gs) else 0L
                    | _ -> failwith "unexpected character"
                ret)

let replicate n =
    let replPattern n =
        let join (s: string seq) = System.String.Join('?', s)
        Seq.replicate n >> Seq.map Array.ofSeq >> Seq.map System.String >> join >> List.ofSeq
    let replGroups n = Seq.replicate n >> Seq.concat >> List.ofSeq
    Seq.map (tmap2 (replPattern n) (replGroups n))

let solve = Seq.map arrange >> Seq.sum

let parseInput = Seq.map ((splitOn " ") >> tuple >> tmap2 id (splitOn "," >> Seq.map int))
let (r1, r2) = AOC.Inputs.load "2023" "12" |> Async.RunSynchronously |> (parseInput >> applyBoth (replicate 1 >> solve) (replicate 5 >> solve))
printfn "Part1: %i" r1
printfn "Part2: %i" r2

