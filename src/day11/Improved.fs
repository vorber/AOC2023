open AOC.Misc

let parseInput =
    let rowCount = Seq.map (Seq.sumBy (fun c -> if c = '#' then 1L else 0L))
    let colCount = Seq.transpose >> rowCount
    Seq.map (fun line -> seq { yield! line }) >> applyBoth rowCount colCount

let expandedDistanceSum expansionFactor (rc, cc) =
    let starCount = Seq.sum rc
    let folder (seen, sum) count = 
        let seen' = seen + count
        (seen', sum + seen * (starCount - seen) * if count=0L then expansionFactor else 1L)
    let distances = Seq.fold (folder) (0L,0L) >> snd
    (distances rc) + (distances cc)

let part1 = expandedDistanceSum 2L
let part2 = expandedDistanceSum 1000000L
let (r1, r2) = AOC.Inputs.load "2023" "11" |> Async.RunSynchronously |> parseInput |> applyBoth part1 part2

printfn "Part1, but better: %i" r1
printfn "Part2, but better: %i" r2
