namespace AOC
open System.Collections.Generic
module Misc =
    let applyBoth f g x = (f x, g x)
    let splitOn (split:string) (str:string) = str.Split(split)
    let safeSkip count = Seq.indexed >> Seq.filter (fst >> (<=) count) >> Seq.map snd
    let flipLastArgs f a b = f b a
    let tuple (arr:'a array) = (arr[0], arr[1])
    let tmap f (a,b) = (f a, f b)
    let tmap2 f g (a, b) = (f a, g b)

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
