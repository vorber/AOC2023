namespace AOC
module Misc =
    let applyBoth f g x = (f x, g x)
    let splitOn (split:string) (str:string) = str.Split(split)
    let flipLastArgs f a b = f b a
