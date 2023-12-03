open System.Text.RegularExpressions

let surroundWithEmptyLines strings = seq {
    let emptyRow = strings |> Seq.head |> String.map (fun _ -> '.')
    yield emptyRow
    yield! strings
    yield emptyRow
}
let toTuple (threeLines:string array) = (threeLines[0], threeLines[1], threeLines[2])
let input =
    AOC.Inputs.load "2023" "3" 
    |> Async.RunSynchronously
//let input =
//    [|"467..114..";
//      "...*......";
//      "..35..633.";
//      "......#...";
//      "617*......";
//      ".....+.58.";
//      "..592.....";
//      "......755.";
//      "...$.*....";
//      ".664.598..";|] :> seq<string>
    |> surroundWithEmptyLines
    |> Seq.windowed 3
    |> Seq.map toTuple


let numberRx = Regex(@"\d+", RegexOptions.Compiled)
let isSymbol c = 
    match c with
    |'.'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> false
    | _ -> true

let getPartNumbers (prev:string, curr:string, next:string) =
    let lineSize = String.length curr 
    let partNumber (m:Match) = 
        let v, l, p = (m.Value, m.Length, m.Index)
        let hasSymbolNeighbour candidates = 
            candidates
            |> Seq.exists isSymbol
        let lo = System.Math.Clamp(p-1, 0, lineSize)
        let hi = System.Math.Clamp(p+l, 0, lineSize) //F# ranges seem to include upper bound, so no +1
        let neighbours = prev[lo..hi] + curr[lo..hi] + next[lo..hi]
        if hasSymbolNeighbour neighbours then Some v else None
    numberRx.Matches curr
    |> Seq.map partNumber
    |> Seq.choose (Option.map int)


let sumOfAllPartNumbers =
    input
    |> Seq.map (getPartNumbers >> Seq.sum)
    |> Seq.sum

printfn "Sum of all part numbers: %i" sumOfAllPartNumbers

type numberPosition = {Value: int; Start: int; End: int}
let getGearScores (prev:string, curr:string, next:string) =
    let starPositions = curr |> Seq.mapi (fun i c -> if c = '*' then Some i else None) |> Seq.choose id
    let numberPositions = 
        let numberPosition (m:Match) =
            {Value = int m.Value; Start = m.Index; End = m.Index + m.Length - 1}
        let numberPositionsIn line =
            numberRx.Matches line
            |> Seq.map numberPosition
        seq {
            yield! numberPositionsIn prev
            yield! numberPositionsIn curr
            yield! numberPositionsIn next
        }
    let neighbourNumbers starPosition =
        let isNeighbour p = starPosition >= (p.Start-1) && starPosition <= (p.End+1)
        numberPositions |> Seq.filter isNeighbour 
    starPositions
    |> Seq.map (neighbourNumbers >> List.ofSeq)
    |> Seq.filter (fun neighbours -> List.length neighbours = 2)
    |> Seq.map (fun l -> l[0].Value*l[1].Value)

let sumOfAllGearScores = 
    input
    |> Seq.map (getGearScores >> Seq.sum)
    |> Seq.sum

printf "Sum of all gear scores: %i" sumOfAllGearScores
