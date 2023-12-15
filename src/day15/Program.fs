open AOC.Misc

let HASH : (char seq -> int) = Seq.fold (fun s c -> ((s + int c) * 17) % 256) 0
let parse = splitOn ","
let part1 = parse >> Seq.sumBy HASH

type Instruction = Set of (string * int) | Reset of string
let label i = match i with | Set x -> fst x | Reset x -> x
let value i = match i with | Set x -> snd x | Reset x -> 0

let parseInstruction instruction =
    let parseSet = splitOn "=" >> tuple >> (tmap2 id int) >> Set
    let parseReset = String.filter((<>) '-') >> Reset
    match instruction with
    | s when s |> String.exists ((=) '=') -> s |> parseSet
    | s when s |> String.exists ((=) '-') -> s |> parseReset
    | _ -> failwith "unexpected input"

let boxes = parse >> Seq.map parseInstruction >> Seq.groupBy (label >> HASH) 
let foldSameLabel (k, prev) (i, next) =
    match prev, next with
    | Set _, Set(l, b) -> (k, Set(l, b))
    | Set _, Reset l -> (i, Reset l)
    | Reset _, Reset l -> (i, Reset l)
    | Reset _, Set(l, b) -> (i, Set(l, b))

let processSteps = 
    Seq.mapi (fun i s -> (i, s)) 
    >> Seq.groupBy (snd >> label)
    >> Seq.map (snd >> Seq.reduce foldSameLabel >> tmap2 id value)
    >> Seq.sortBy fst
    >> Seq.map snd
    >> Seq.filter ((<>) 0)

let boxScore (i, s) = (1+i) * (s |> Seq.mapi (fun p v -> (p+1) * v) |> Seq.sum)

let part2 = boxes >> Seq.map (tmap2 id processSteps) >> Seq.sumBy boxScore

let (r1, r2) = AOC.Inputs.load "2023" "15" |> Async.RunSynchronously |> Array.head |> applyBoth part1 part2

printfn "%A" r1
printfn "%A" r2
