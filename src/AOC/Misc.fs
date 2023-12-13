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
