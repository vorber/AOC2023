open AOC.Misc

let expansions =
    let isEmpty = Seq.forall ((=) '.')
    let rowsToExpand = Seq.mapi (fun i l -> if isEmpty l then Some (int64 i) else None) >> Seq.choose id
    let colsToExpand = Seq.transpose >> rowsToExpand 
    applyBoth rowsToExpand colsToExpand

let expandStars by ((re, ce), stars) =
    let expandStar (x,y) = 
        let offset c = Seq.where ((>) c) >> Seq.length >> int64 >> (*) by
        (x + offset x ce, y + offset y re)
    stars |> Seq.map expandStar

let stars = 
    let starPos y x c = if c = '#' then Some (int64 x, int64 y) else None
    let starsInLine y = Seq.mapi (starPos y) >> Seq.choose id
    Seq.mapi starsInLine >> Seq.concat

let allDistances s =
    let distance (p1, p2) = abs(fst p1 - fst p2) + abs(snd p1 - snd p2)
    (s,s) ||> Seq.allPairs |> Seq.map distance //TODO: we count all distances twice. Once should be enough

let parseInput = Seq.map (fun line -> seq { yield! line }) >> applyBoth expansions stars

let part1 = (expandStars 1L)  >> allDistances >> Seq.sum
let part2 = (expandStars 999999L) >> allDistances >> Seq.sum

let (r1, r2) = AOC.Inputs.load "2023" "11" |> Async.RunSynchronously |> parseInput |> applyBoth part1 part2

printfn "Part1: %i" (r1 / 2L)
printfn "Part2: %i" (r2 / 2L)
