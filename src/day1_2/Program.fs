    open System.Text.RegularExpressions
    let rxStr = @"[0-9]|one|two|three|four|five|six|seven|eight|nine"
    let rx = Regex(rxStr, RegexOptions.Compiled)
    let revRx = Regex(rxStr, RegexOptions.Compiled ||| RegexOptions.RightToLeft)
    
    let readLines filePath = System.IO.File.ReadLines(filePath)
    
    let combine pair =
        match pair with
        | (f, l) -> 10*f + l 
    
    let toDigit s =
        match s with
        | "1" | "one" -> 1
        | "2" | "two" -> 2
        | "3" | "three" -> 3
        | "4" | "four" -> 4
        | "5" | "five" -> 5
        | "6" | "six" -> 6
        | "7" | "seven" -> 7
        | "8" | "eight" -> 8
        | "9" | "nine" -> 9
        | "0" | "zero" -> 0
        | _ -> 0
    
    let getCalibration line =
        let firstMatch = rx.Match line
        let lastMatch = revRx.Match line
        (toDigit firstMatch.Value, toDigit lastMatch.Value) 
        |> combine
    
    let sumOfAllCalibrations = 
        readLines "day1_2.input"
        |> Seq.map getCalibration
        |> Seq.sum
    
    printf "%i\n" sumOfAllCalibrations
