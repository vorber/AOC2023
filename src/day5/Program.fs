open System.Text.RegularExpressions
let testInput = 
    [|
        "seeds: 79 14 55 13";
        "";
        "seed-to-soil map:";
        "50 98 2";
        "52 50 48";
        "";
        "soil-to-fertilizer map:";
        "0 15 37";
        "37 52 2";
        "39 0 15";
        "";
        "fertilizer-to-water map:";
        "49 53 8";
        "0 11 42";
        "42 0 7";
        "57 7 4";
        "";
        "water-to-light map:";
        "88 18 7";
        "18 25 70";
        "";
        "light-to-temperature map:";
        "45 77 23";
        "81 45 19";
        "68 64 13";
        "";
        "temperature-to-humidity map:";
        "0 69 1";
        "1 0 69";
        "";
        "humidity-to-location map:";
        "60 56 37";
        "56 93 4";
    |] |> List.ofArray

let input = AOC.Inputs.load "2023" "5" |> Async.RunSynchronously |> List.ofArray

let numberLineRx = Regex(@"^(?<dst>\d+) (?<src>\d+) (?<cnt>\d+)$", RegexOptions.Compiled)
let mapRx = Regex(@"^(?<src>\w+)-to-(?<dst>\w+) map:$", RegexOptions.Compiled)

type Range = {Start:int64; Count:int64}
type RangeMap = {SrcStart:int64; DstStart:int64;Count:int64}
type Mapping = {Src:string; Dst:string; Mappers: RangeMap list}
type Spec = {InitialCategory: string; InitialValues: Range list; Maps: Map<string, Mapping>}

let splitBy predicate lines = 
    let processLine line (acc, res) = 
        let acc' = line::acc
        if predicate line then ([], acc'::res)
        else (acc', res)
    List.foldBack processLine lines ([], []) |> snd |> List.map List.tail

let parseSpec (lines:string list list) = 
    let parseInitial (line:string) = 
        let parts = line.Split("s: ")
        let stringValues = parts[1].Split(" ") |> List.ofArray
        let values = 
            stringValues 
            |> List.chunkBySize 2
            |> List.collect List.pairwise
            |> List.map (fun v -> {Start=int64 (fst v); Count=int64 (snd v)})
        (parts[0], values)
    let parseMaps (maps:string list list) =
        let parseMap (mapLines:string list) = 
            let h = mapRx.Match(mapLines.Head)
            let g (m:Match) (s:string) = m.Groups[s].Value 
            let lineToMap line =
                let m = numberLineRx.Match(line)
                {SrcStart = g m "src" |> int64; DstStart = g m "dst" |> int64; Count = g m "cnt" |> int64}
            { Src = g h "src"; Dst = g h "dst"; Mappers = mapLines |> List.tail |> List.map lineToMap }
        maps 
        |> List.map parseMap
    let parse initial (maps: Mapping list) = 
        {
            InitialCategory = initial |> fst;
            InitialValues = initial |> snd;
            Maps = maps |> List.map (fun m -> (m.Src, m)) |> Map.ofList
        }
    match lines with
    | [] -> failwith "unexpected input"
    | x::xs -> parse (parseInitial x.Head) (parseMaps xs)


type MapMark = {Shift:int64; Pos:int64}
let mapStartMark (r:RangeMap) =
    {Shift=r.DstStart - r.SrcStart; Pos=r.SrcStart}
let mapEndMark (r:RangeMap) = 
    {Shift=r.DstStart - r.SrcStart; Pos=r.SrcStart + r.Count - 1L}

type Mark =
    RangePoint of int64 | RangeStart of int64 | RangeEnd of int64 | MapStart of MapMark | MapEnd of MapMark | MapPoint of MapMark

let pos (mark:Mark) =
    match mark with 
    | RangePoint p -> p
    | RangeStart p -> p
    | RangeEnd p -> p
    | MapPoint m -> m.Pos
    | MapStart m -> m.Pos
    | MapEnd m -> m.Pos
    
let mapMarks (r:RangeMap) =
    let s = mapStartMark r
    let e = mapEndMark r
    if s.Pos = e.Pos then [MapPoint s] else [MapStart s; MapEnd e]
let rangeMarks (r:Range) =
    if r.Count = 0 then [] 
    elif r.Count = 1 then [RangePoint r.Start] 
    else [RangeStart r.Start; RangeEnd (r.Start + r.Count - 1L)]

type Interval = Active of int64 | Inactive 
let remap (rl:Range list) (mapping:Mapping) =
    let rmarks = rl |> List.collect rangeMarks
    let mmarks = mapping.Mappers |> List.collect mapMarks 
    let allMarks = rmarks@mmarks |> List.sortBy pos
    let rec loop state shift acc marks =
        match marks with
        | [] -> acc
        | x::xs -> 
            match x with
            | RangePoint p -> loop state shift ({Start=p+shift; Count=1}::acc) xs
            | RangeStart p -> loop (Active p) shift acc xs 
            | RangeEnd p -> 
                let (Active x) = state
                loop Inactive shift ({Start = x + shift; Count = p - x + 1L}::acc) xs
            | MapPoint m -> 
                match state with
                | Active p -> 
                    loop (Active (m.Pos + 1L)) 0L ({Start = p; Count = m.Pos - p}::{Start = m.Pos + m.Shift; Count = 1}::acc) xs
                | Inactive -> loop state 0L acc xs
            | MapStart m ->
                match state with
                | Active p -> loop (Active m.Pos) m.Shift ({Start = p; Count = m.Pos - p}::acc) xs
                | Inactive -> loop Inactive m.Shift acc xs
            | MapEnd m -> 
                match state with
                | Active p -> loop (Active (m.Pos + 1L)) 0 ({Start = p+shift; Count = m.Pos - p + 1L}::acc) xs
                | Inactive -> loop Inactive 0L acc xs
    loop Inactive 0 [] allMarks //TODO: should be possible to replace with fold

let spec =
    ""::input
    |> splitBy (System.String.IsNullOrWhiteSpace)
    |> parseSpec 

let rec follow cat (v:Range list) =
    match spec.Maps |> Map.tryFind cat with
    | None -> v
    | Some m -> (remap v m) |> (follow m.Dst)

let res = 
    follow spec.InitialCategory (spec.InitialValues)
    |> List.map (fun r -> r.Start)
    |> List.min

printfn "Lowest location number for initial seeds: %i" res
