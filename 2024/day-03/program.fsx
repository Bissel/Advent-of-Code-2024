#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

let data = Utilities.getData("./day-03/example.txt", " ")
         |> Seq.map (Array.map int)
let result1 = ""
printfn $"Result 1: {result1}"

let result2 = ""
printfn $"Result 2: {result2}"

exit 0