#load @"../AdventOfCode/Utilities.fs"

open System.Text.RegularExpressions
open AdventOfCode

let data = Utilities.readLines("./day-03/input.txt")
         |> (String.concat "")

// However, because the program's memory has been corrupted, there are also many invalid characters that
// should be ignored, even if they look like part of a mul instruction. Sequences like
// mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.

let findIndices (content: string, pattern: string) =
    seq {
        for m in Regex.Matches(content, pattern) do
            yield m.Index
    }

let findMultiply (content: string) =
    seq {
        for m in Regex.Matches(content, "mul\(\d+,\d+\)") do
            yield (m.Value, m.Index)
    }

let getNumbers (multiplications: string) =
    seq {
        for m in Regex.Matches(multiplications, "\d+") do
            yield int m.Value
    } |> Seq.toArray |> fun numbers -> (numbers[0], numbers[1])

let multiplications = data |> findMultiply

let products (sequence: (string * int) seq)
                = sequence                  
                |> Seq.map fst
                |> Seq.map getNumbers
                |> Seq.map (fun (a,b) -> a * b)
                
let result1 = (products multiplications) |> Seq.sum
printfn $"Result 1: {result1}"

// There are two new instructions you'll need to handle:
//  - The do() instruction enables future mul instructions.
//  - The don't() instruction disables future mul instructions.
// Only the most recent do() or don't() instruction applies.
// At the beginning of the program, mul instructions are enabled.

let doIndices = Seq.append [0] (findIndices (data, "do\(\)"))
let dontIndices = Seq.append (findIndices (data, "don't\(\)")) [data.Length]

let enabled =
    seq {
        for i in doIndices do
            let endIndex = dontIndices |> Seq.find (fun a -> a > i)
            yield (i, endIndex)
    }
    |> Seq.toList

let inEnabled (i: int) = enabled
                         |> Seq.exists (fun (startIdx, endIdx) -> i >= startIdx && i <= endIdx)

let enabledMultiplications
    = multiplications
      |> Seq.where (fun (_, i) -> inEnabled i)

let result2 = (products enabledMultiplications) |> Seq.sum
printfn $"Result 2: {result2}"

exit 0