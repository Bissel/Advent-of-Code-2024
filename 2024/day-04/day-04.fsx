#load @"../AdventOfCode/Utilities.fs"

open System
open AdventOfCode

let search = "XMAS"

module String =
    let rev (s: string) = s |> Seq.rev |> String.Concat
    let replaceNotAllowedChars (allowed: string) (replacement: char) (s: string) 
      = s |> Seq.map (fun c -> if allowed.Contains(c) then c else replacement ) |> String.Concat
      
    let pad (count: int) = String.replicate count " "

let data = Utilities.readLines("./day-04/input.txt")
            |> Seq.map (String.replaceNotAllowedChars search '.')

// This word search allows words to be
// - horizontal,
// - vertical,
// - diagonal,
// - written backwards, or even
// - overlapping other words.

let rec countRow (row: char list)
  = match row with
     | [] | [_;_;_] -> 0
     | 'X' :: 'M' :: 'A' :: 'S' :: tail -> 1 + (countRow tail)
     | _ :: tail -> countRow tail

let count (rows: string seq)
 = rows
    |> Seq.map (fun row -> countRow (Seq.toList row)) 
    |> Seq.sum
    
let reverse (rows: string seq)
  = rows |> Seq.map String.rev

let permutationVertical (rows: string seq) = seq {
    let colCount = String.length (Seq.head rows)
    
    for colIndex in [0 .. colCount - 1] do
        yield seq {
                for row in rows do
                    yield row[colIndex]
            } |> String.Concat
}



let permutationDiagonal (rows: string seq) (offset: int) = 
    let rowCount = Seq.toList rows |> List.length
    let paddedString (index: int, s: string)
      = if offset < 0
          then (String.pad (rowCount - (abs offset) * index)) + s + (String.pad ((abs offset) * index))
          else (String.pad (offset * index)) + s + (String.pad (rowCount - offset * index))
    
    rows |> Seq.indexed |> Seq.map paddedString |> permutationVertical

let verticalData = permutationVertical data |> Seq.toList
let diagonalRData = permutationDiagonal data +1 |> Seq.toList
let diagonalLData = permutationDiagonal data -1 |> Seq.toList

let countH = (count data) + (count (reverse data))
let countV = (count verticalData) + (count (reverse verticalData))
let countDR = (count diagonalRData) + (count (reverse diagonalRData))
let countDL = (count diagonalLData) + (count (reverse diagonalLData))

// For the example:
printfn $"H: {countH} = 5"
printfn $"V: {countV} = 3"
printfn $"DL: {countDL} = 5"
printfn $"DR: {countDR} = 5"

let result1 = countH + countV + countDR + countDL

printfn $"Result 1: {result1}"

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0