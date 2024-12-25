#load @"../AdventOfCode/Utilities.fs"

type Key  = (int*int*int*int*int)
type Lock = (int*int*int*int*int)


open System
open AdventOfCode

let data =
    Utilities.readLines "./day-25/example.txt" 

let result1 = "-"

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0