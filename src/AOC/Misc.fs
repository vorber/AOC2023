namespace AOC
open System.Collections.Generic
module Misc =
    let applyBoth f g x = (f x, g x)
    let splitOn (split:string) (str:string) = str.Split(split)
    let safeSkip count = Seq.indexed >> Seq.filter (fst >> (<=) count) >> Seq.map snd
    let flipLastArgs f a b = f b a
    let flip f a b = f b a
    let tuple (arr:'a array) = (arr[0], arr[1])
    let triple (arr:'a array) = (arr[0], arr[1], arr[2])
    let dup x = (x,x)
    let tmap f (a,b) = (f a, f b)
    let tmap2 f g (a, b) = (f a, g b)
    let Const a _ = a
    let uncurry f (a, b) = f a b
    let curry f a b = f (a, b)
    let (<&>) f g x = f x && g x

    let memoize f =
        let cache = Dictionary<_, _>();
        fun c ->
            match cache.TryGetValue c with 
            | true, value -> value
            | false, _ -> 
                let value = f c
                cache.Add(c, value)
                value

    let splitWhen (predicate: 'a -> bool) (elements: 'a list) =
        let split l (a, t) =
            if predicate l then ([], a::t)
            else (l::a, t)
        List.foldBack split elements ([], []) |> fun (a, t) -> a::t

    let splitBefore (predicate: 'a -> bool) (elements: 'a list) =
        let split l (a, t) =
            if predicate l then ([], (l::a)::t)
            else (l::a, t)
        List.foldBack split elements ([], []) |> fun (a, t) -> a::t

    let countBits64 n =
        let rec count acc n =
            match n with
            | 0L -> acc
            | x -> count (acc+1) (x &&& (x-1L))
        count 0 n

    type Direction = 
        Up | Down | Left | Right
        
        member this.tuple = 
            match this with
            | Up -> (0, -1)
            | Down -> (0, 1)
            | Left -> (-1, 0)
            | Right -> (1, 0)
        member this.rev = match this with | Up -> Down | Down -> Up | Left -> Right | Right -> Left

        static member parse s = 
            match s with 
            | "U" -> Up
            | "D" -> Down
            | "L" -> Left
            | "R" -> Right
            | unexpected -> failwithf "unexpected input %s" unexpected

        static member move (x, y) distance dir =
            match dir with
            | Up -> (x, y-distance)
            | Down -> (x, y+distance)
            | Left -> (x-distance, y)
            | Right -> (x+distance, y)

        static member all = [Up; Down; Left; Right]

    module Set =
        let isNonEmptySubset s1 s2 = not (Set.isEmpty s1) && Set.isSubset s1 s2 
        let (|Empty|NonEmpty|) s = if Set.isEmpty s then Empty else NonEmpty s

    module Map =
        let tryFind2 k1 k2 (m:Map<'a, Map<'b, 'c>>) = Map.tryFind k1 m |> Option.bind (Map.tryFind k2)
