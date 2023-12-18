open AOC.Misc

type Instruction = {Dir: Direction; Dist:int; Color: string}

let dig =
    let points = Seq.scan (fun pos step -> step.Dir.move step.Dist pos) (0, 0)
    let perimeter = Seq.sumBy (fun step -> step.Dist)
    applyBoth points perimeter
 
let doublearea : ((int*int) seq -> int64) = Seq.map (tmap int64) >> Seq.pairwise >> Seq.fold (fun s ((x1, y1), (x2, y2)) -> s + x1*y2 - x2*y1 ) 0L  >> abs
let solve = dig >> tmap2 doublearea id >> (fun (a, p) -> a/2L + (int64 p)/2L + 1L)

let parseP1 = 
    let parseTokens (tokens:string array) = {Dir = Direction.parse tokens[0]; Dist = int tokens[1]; Color = tokens[2]}
    Seq.map ((splitOn " ") >> parseTokens)

let parseP2 =
    let strip (s:string) = s.Replace("(", "").Replace(")", "").Replace("#", "")
    let parseHexCode (s:char array) = 
        {
            Dir = match Array.last s with
                    | '0' -> Right
                    | '1' -> Down
                    | '2' -> Left
                    | '3' -> Up
                    | d -> failwithf "unexpected %c" d;
            Dist = System.Int32.Parse(s[..s.Length - 2], System.Globalization.NumberStyles.HexNumber);
            Color = "";
        }
    Seq.map ((splitOn " ") >> Array.last >> strip >> Array.ofSeq >> parseHexCode)

let part1 = parseP1 >> solve
let part2 = parseP2 >> solve

let (r1, r2) = AOC.Inputs.load "2023" "18" |> Async.RunSynchronously |> applyBoth part1 part2

printfn "%A" r1
printfn "%A" r2
