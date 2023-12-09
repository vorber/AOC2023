open AOC.Misc

let predict folder = 
    let allZero = Array.forall ((=) 0)
    let diff: (int array -> int array) = Array.pairwise >> Array.map (fun (a, b) -> b - a)
    let generator s = if (allZero s) then None else Some (s, diff s)
    Array.unfold generator >> flipLastArgs (Array.foldBack (folder)) 0

let parseLine = splitOn " " >> Array.map int 
let folder = flipLastArgs Array.get 1 >> (-)
let part1 = Seq.map (Array.rev >> predict folder) >> Seq.sum
let part2 = Seq.map (predict folder) >> Seq.sum
let r1,r2 = AOC.Inputs.load "2023" "9" |> Async.RunSynchronously |> Seq.map parseLine|> applyBoth part1 part2

printfn "Part1: %i" r1
printfn "Part2: %i" r2
