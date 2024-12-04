#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

let data = Utilities.readLines("./day-04/example.txt")
         |> (String.concat "")

let result1 = "-"
printfn $"Result 1: {result1}"

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0