open AOC.Misc

let reflection smudges xs =
    let len = xs |> Array.length
    let rs = xs |> Array.rev
    let candidate (a:int64 array) i =
        let left i = a[..i] |> Array.rev 
        let right i = a[i+1..] 
        let pairs = (left i, right i) |> tmap (Array.take (min (i+1) (len-i-1))) ||> Array.zip
        let diff = Array.map (fun (a, b) -> countBits64 (a^^^b)) >> Array.sum
        if Array.isEmpty pairs then None else
            match diff pairs with
            | x when x = smudges -> Some i
            |  _ -> None
    let pick i = (candidate xs i) |> Option.orElse (candidate rs i |> Option.map (fun v -> len - v - 2))
    [0..len-1] |> List.tryPick pick 

let parsePattern = 
    let toint64 s = System.Convert.ToInt64(s, 2)
    let rows :(string seq -> int64 seq) = Seq.map (fun s -> s.Replace("#", "1").Replace(".", "0")) >> Seq.map toint64
    let cols :(string seq -> int64 seq) = Seq.transpose >> Seq.map (System.String.Concat) >> rows
    applyBoth rows cols

let parseInput = List.ofSeq >> splitWhen (System.String.IsNullOrWhiteSpace) >> List.map parsePattern >> List.map (tmap Array.ofSeq)

let score (r,c) = 
    match r, c with
    | None, Some x -> (x+1)
    | Some x, None -> 100*(x+1)
    | _, _ -> failwith (sprintf "unexpected %O %O" r c)

let solve smudge = List.map (tmap (reflection smudge)) >> List.map score >> List.sum

let solution = parseInput >> applyBoth (solve 0) (solve 1)

let (r1, r2) = AOC.Inputs.load "2023" "13" |> Async.RunSynchronously |> solution
printfn "Part1: %i\nPart2: %i" r1 r2
