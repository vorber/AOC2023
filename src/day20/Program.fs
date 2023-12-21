open AOC.Misc
open FParsec

type Label = Label of string
type PulseType = High | Low

type Pulse = {Source: Label; Destination: Label; Type: PulseType}

type FlipFlopState = 
    On | Off
    member this.Flip = match this with | On -> Off | Off -> On

type FlipFlop = 
    {State: FlipFlopState; Destinations: Label list}
    member this.Flip = {this with State = this.State.Flip}

type Nand = 
    {SourcePulses : Map<Label, PulseType>; Destinations : Label list}
    member this.Update pulse = {this with SourcePulses = this.SourcePulses |> Map.add pulse.Source pulse.Type}

type Broadcaster = {Destinations: Label list}
type Button = {Destinations: Label list}

type Module = 
    | FlipFlop of FlipFlop
    | Conjunction of Nand
    | Broadcaster of Broadcaster
    | Button of Button
    | Nop

    member this.Destinations =
        match this with 
        | FlipFlop f -> f.Destinations
        | Conjunction c -> c.Destinations
        | Broadcaster b -> b.Destinations
        | Button b -> b.Destinations
        | Nop -> []
    
    member this.PulseType =
        match this with
        | FlipFlop f -> match f.State with | On -> High | Off -> Low
        | Conjunction c -> if c.SourcePulses |> Map.values |> Seq.forall ((=) High) then Low else High //TODO: maybe keep a list instead of map and just go over?
        | Broadcaster _ -> Low
        | Button _ -> Low
        | Nop -> failwith "something went terribly wrong"

    member this.Pulse p = 
        let propagate, updated =
            match this with
            | FlipFlop f when p.Type = Low -> true, FlipFlop f.Flip
            | FlipFlop f -> false, FlipFlop f
            | Conjunction c -> true, Conjunction (c.Update p)
            | Nop -> false, this
            | _ -> true, this
        let pulses = if propagate then this.Destinations |> List.map (fun d -> {Source = p.Destination; Destination = d; Type = updated.PulseType}) else []
        in
        updated, pulses

let pushButton modules until =
    let buttonPulse = {Source = Label "button"; Destination = Label "broadcaster"; Type = Low}
    let pushOnce modules = 
        let processPulse modules p =
            let m = modules |> Map.tryFind p.Destination |> Option.defaultValue Nop
            let m', ps = m.Pulse p
            (ps, modules |> Map.add p.Destination m')
        let nextState (pulses, modules) =
            match pulses with 
            | [] -> None
            | p::ps ->
                let (p', m') = processPulse modules p
                Some ((p, m'), (ps@p', m'))
        Seq.unfold nextState ([buttonPulse], modules) |> applyBoth (Seq.map fst) (Seq.last >> snd)
    let nextPush (pushCount, modules, pulses) =
        match until (pushCount, modules, pulses) with
        | true -> None
        | false -> 
            let pulses', modules' = pushOnce modules
            Some ((pulses', pushCount), (pushCount + 1, modules', pulses'))
    in
    (1, modules, Seq.empty) |> Seq.unfold nextPush |> applyBoth (Seq.collect fst) (Seq.map snd) 

let parseModule = 
    let parseLabel : Parser<Label, string> = many1Chars asciiLetter |>> Label
    let parseFlipFlop : Parser<Label list -> Label*Module, string> = 
        let make label dests = label, FlipFlop {State=Off; Destinations=dests}
        pchar '%' >>. parseLabel |>> make
    let parseConjunction : Parser<Label list -> Label*Module, string> =
        let make label dests = label, Conjunction {SourcePulses = Map.empty; Destinations = dests}
        pchar '&' >>. parseLabel |>> make
    let parseBroadCaster : Parser<Label list -> Label*Module, string> =
        let make label dests = Label label, Broadcaster {Broadcaster.Destinations = dests}
        pstring "broadcaster" |>> make
    let parseDestinations : Parser<Label list, string> =
        sepBy1 parseLabel (skipString ", ")
    in    
    choice [parseFlipFlop; parseConjunction; parseBroadCaster] .>>. (skipString " -> " >>. parseDestinations .>> skipNewline)
    |>> fun (makeModule, destinations) -> makeModule destinations
    
let connectConjunctions (modules: (Label*Module) list) = 
    let sources l =
        modules 
        |> Seq.filter (snd >> _.Destinations >> (List.contains l))
        |> Seq.map fst
        |> Seq.map (fun l -> (l, Low))
        |> Map
    let connect (l, m:Module) = l, Conjunction {SourcePulses = (sources l); Destinations = m.Destinations}
    let c, r = modules |> List.partition (fun m -> match snd m with | Conjunction _ -> true | _ -> false)
    let connected = c |> List.map connect
    in
    connected @ r

let p1 modules = 
    let ntimes n = fun (cnt, _, _) -> cnt > n
    let countPulses = fst >> Seq.groupBy (_.Type) >> Seq.map (snd >> Seq.length) >> Seq.reduce (*)
    pushButton modules (ntimes 1000) |> countPulses

let p2 modules = 
    let rxModulePredecessor: Map<Label, Module> -> Module = 
        Map.values >> Seq.filter (_.Destinations >> List.contains (Label "rx")) >> Seq.exactlyOne
    let predSources = match (rxModulePredecessor modules) with
                        | Conjunction c -> c.SourcePulses |> Map.keys 
                        | _ -> failwith "should not happen, ever :P"
    let receivedLowPulse label = fun (_, _, pulses) -> pulses |> Seq.exists (fun p -> p.Destination = label && p.Type = Low)
    let pushesToReceiveLowPulse label = pushButton modules (receivedLowPulse label) |> snd |> Seq.last
    let periods = predSources |> Seq.map (pushesToReceiveLowPulse >> bigint)
    let lcm a b = a * b / bigint.GreatestCommonDivisor(a,b)
    in
    periods |> Seq.reduce lcm 

let r1, r2 = 
    AOC.Inputs.parse "2023" "20" (many1 parseModule .>> eof) 
    |> connectConjunctions
    |> Map
    |> applyBoth p1 p2

printfn "%A" r1
printfn "%A" r2
