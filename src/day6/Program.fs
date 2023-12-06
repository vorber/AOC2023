open System.Text.RegularExpressions

let numberRx = Regex(@"\d+", RegexOptions.Compiled)

let testInput = [|
                    "Time:      7  15   30";
                    "Distance:  9  40  200";
                |]

type Race = {Time:double; Distance:double}

let parseInputP1 (line1:string) (line2:string) =
    let times = numberRx.Matches(line1) |> Seq.map (fun m -> m.Value |> double) 
    let distances = numberRx.Matches(line2) |> Seq.map (fun m -> m.Value |> double) 
    (times, distances)
    ||> Seq.zip
    |> Seq.map (fun x -> {Time=fst x; Distance=snd x})

let integerPointsBetweenRoots a b c =
    let d = sqrt (b*b - 4.0*a*c)
    let roots = [0.5*(-b + d); 0.5*(-b - d)]
    let low = roots |> List.min |> System.Math.Ceiling
    let hi = roots |> List.max |>  System.Math.Floor
    int (hi - low) + 1

let winningTimes (r:Race) =
    integerPointsBetweenRoots 1 -r.Time r.Distance

let input = AOC.Inputs.load "2023" "6" |> Async.RunSynchronously

let result1 = 
    parseInputP1 input[0] input[1]//testInput[0] testInput[1]
    |> Seq.map winningTimes
    |> Seq.fold (fun acc x -> acc * x) 1

printfn "Part1: %i" result1

let parseInputP2 (line1:string) (line2:string) =
    let time = line1.Substring("Times: " |> String.length).Replace(" ", "") |> double
    let distance = line2.Substring("Distance: " |> String.length).Replace(" ", "") |> double
    {Time=time; Distance=distance}

let result2 = 
    (parseInputP2 input[0] input[1]) |> winningTimes

printfn "Part2: %i" result2
