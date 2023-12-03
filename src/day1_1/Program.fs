let readLines filePath = System.IO.File.ReadLines(filePath)

let printLine line = printf "%s\n" line
let printSum sum  = printf "%i\n" sum

let  (|Digit|NonDigit|) c = if  c >= '0' &&  c <= '9' then Digit else NonDigit

let chooseDigit c =
    match c with
    | Digit -> Some c
    | _ -> None 

let digitsOnly line = line |> Seq.choose chooseDigit

let (|SeqEmpty|SeqCons|) xs = if Seq.isEmpty xs then SeqEmpty else SeqCons(Seq.head xs, Seq.tail xs)

let firstAndLastDigits digits =
    match digits with
    | SeqEmpty -> ('*', '*')
    | SeqCons(head, SeqEmpty) -> (head, head)
    | SeqCons(head,  tail) -> (head, tail |> Seq.last)

let combine pair =
    match pair with
    | (f, l) -> sprintf "%c%c" f l


let processLine line =
    line
    |> digitsOnly
    |> firstAndLastDigits
    |> combine

let sumOfAllCalibrations = 
    readLines "day1_1.input"
    |> Seq.map processLine
    |> Seq.map int
    |> Seq.sum

printf "%i\n" sumOfAllCalibrations
