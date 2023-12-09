open AOC.Misc

let predict = 
    let allZero = Array.forall ((=) 0)
    let diff: (int array -> int array) = Array.pairwise >> Array.map (fun (a, b) -> b - a)
    let generator s = if (allZero s) then None else Some (s, diff s)
    let folder = flipLastArgs Array.get 0 >> (-)
    Array.unfold generator >> flipLastArgs (Array.foldBack (folder)) 0

let part1 = Seq.sumBy (Array.rev >> predict)
let part2 = Seq.sumBy predict

let parseLine = splitOn " " >> Array.map int 
let r1,r2 = AOC.Inputs.load "2023" "9" |> Async.RunSynchronously |> Seq.map parseLine|> applyBoth part1 part2

printfn "Part1: %i" r1
printfn "Part2: %i" r2
