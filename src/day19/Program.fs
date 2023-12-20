open AOC.Misc
open System.Text.RegularExpressions

type Range = (int*int) 

let splitBelow a (l,h) =
    if l >= a then (None, Some (l,h))
    elif h < a then (Some (l,h), None)
    else (Some(l,a-1), Some(a, h))

let splitAbove a (l,h) =
    if h <= a then (None, Some (l,h))
    elif l > a then (Some (l,h), None)
    else (Some(a+1, h), Some(l,a))

type Rating = {X: int; M: int; A: int; S: int}

type RatingRange = {X: Range; M: Range; A: Range; S: Range}
let toRange (r:Rating) = {RatingRange.X = (r.X, r.X); M = (r.M, r.M); A = (r.A, r.A); S = (r.S, r.S)}
type Condition = RatingRange -> (RatingRange option)*(RatingRange option)

type RuleResult = Label of string | Accept | Reject
type ConditionalRule = {Condition: Condition; Result: RuleResult}
type Rule = Conditional of ConditionalRule | Unconditional of RuleResult

let parse = 
    let ratingRx = Regex(@"{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)}", RegexOptions.Compiled)
    let parseRatings = 
        let get (m:Match) (c:string) = m.Groups[c].Value |> int
        let fromMatch m = {Rating.X= get m "x"; M = get m "m"; A = get m "a"; S = get m "s"}
        Seq.map (ratingRx.Match >> fromMatch)
    let parseWorkflows = 
        let parseWorkflow = 
            let strip (s:string) = s.Replace("}", "")
            let parseRules = 
                let parseRule s = 
                    let parseCondition (cs:string) = 
                        let (cat, op, v) = (cs[0], cs[1], int cs[2..])
                        let split = match op with 
                                     | '<' -> splitBelow v
                                     | '>' -> splitAbove v
                                     | o -> failwithf "unexpected op: %c" o
                        match cat with
                                 | 'x' -> fun r -> (split r.X) |> tmap (Option.map(fun q -> {X=q; M=r.M; A=r.A; S=r.S}))
                                 | 'm' -> fun r -> (split r.M) |> tmap (Option.map(fun q -> {X=r.X; M=q; A=r.A; S=r.S}))
                                 | 'a' -> fun r -> (split r.A) |> tmap (Option.map(fun q -> {X=r.X; M=r.M; A=q; S=r.S}))
                                 | 's' -> fun r -> (split r.S) |> tmap (Option.map(fun q -> {X=r.X; M=r.M; A=r.A; S=q}))
                                 | c -> failwithf "unexpected category: %c" c
                    let makeResult s = match s with | "A" -> Accept | "R" -> Reject | l -> Label l
                    let makeConditional (c, l) = Conditional {Condition = c; Result = makeResult l}
                    let makeUnconditional = makeResult >> Unconditional
                    in
                    if s |> String.forall ((<>) ':') then makeUnconditional s
                    else s |> splitOn ":" |> tuple |> tmap2 parseCondition id |> makeConditional
                splitOn "," >> Seq.map parseRule
            strip >> splitOn "{" >> tuple >> tmap2 id parseRules
        Seq.map parseWorkflow >>  Map
    splitWhen (System.String.IsNullOrWhiteSpace) >> Array.ofList >> tuple >> tmap2 parseWorkflows parseRatings

let rec accepted (workflows:Map<string, Rule seq>) (range, rules) =
    let processRule range rule =
        let splitByRule rule range =
            match rule with
            | Unconditional result -> (Some (range, result), None)
            | Conditional cr -> range |> cr.Condition |> tmap2 (Option.map (fun l -> (l, cr.Result))) id
        let (take, skip) = splitByRule rule range
        let acptd = match take with 
                    | None -> []
                    | Some (r, res) -> 
                        match res with
                        | Accept -> [r]
                        | Reject -> []
                        | Label s -> accepted workflows (r, workflows[s])
        (acptd, skip)
    let folder (acc, next) rule = 
        match next with 
        | None -> (acc, next) 
        | Some range -> 
            let (acc', next') = processRule range rule
            (acc'@acc, next')
    (([], Some range), rules) ||> Seq.fold folder |> fst

let part1 workflows = 
    Seq.map toRange 
    >> Seq.collect (fun r -> accepted workflows (r, workflows["in"])) 
    >> Seq.sumBy (fun r -> fst r.X + fst r.M + fst r.A + fst r.S)
    
let part2 (workflows: Map<string, Rule seq>) = 
    let rangeSize r =
        let d (l:int,h:int) = bigint h - bigint l + 1I
        (d r.X)*(d r.M)*(d r.A)*(d r.S)
    accepted workflows >> Seq.sumBy rangeSize

let (workflows, ratings) = AOC.Inputs.load "2023" "19" |> Async.RunSynchronously |> List.ofSeq |> parse
ratings |> part1 workflows |> (printfn "%A")
({RatingRange.X = (1,4000); M = (1,4000); A = (1,4000); S = (1,4000)}, workflows["in"]) |> (part2 workflows) |> printfn "%A"

