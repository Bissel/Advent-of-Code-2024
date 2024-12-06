#load @"../AdventOfCode/Utilities.fs"

open System
open AdventOfCode

module String =
    let rev (s: string) = s |> Seq.rev |> String.Concat
    let replaceNotAllowedChars (allowed: string) (replacement: char) (s: string) 
      = s |> Seq.map (fun c -> if allowed.Contains(c) then c else replacement ) |> String.Concat
      
    let pad (count: int) = String.replicate count " "

let data = Utilities.readLines("./day-04/input.txt")

// This word search allows words to be
// - horizontal,
// - vertical,
// - diagonal,
// - written backwards, or even
// - overlapping other words.

let rec countXmas (row: char list)
  = match row with
     | [] | [_;_;_] -> 0
     | 'X' :: 'M' :: 'A' :: 'S' :: tail -> 1 + (countXmas tail)
     | _ :: tail -> countXmas tail

let count (rows: string seq)
 = rows
    |> Seq.map (fun row -> countXmas (Seq.toList row)) 
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
// printfn $"H: {countH} = 5"
// printfn $"V: {countV} = 3"
// printfn $"DL: {countDL} = 5"
// printfn $"DR: {countDR} = 5"

let result1 = countH + countV + countDR + countDL

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let group3x3 (rows: string seq) = seq {
    let width = rows |> Seq.head |> _.Length
    let height = rows |> Seq.toList |> _.Length
    
    let matrix = Utilities.toMatrix rows
        
    let inRange X Y (x,y,_) =
           (x = X || (x - 1) = X || (x + 1) = X)
        && (y = Y || (y - 1) = Y || (y + 1) = Y)
        
    for x in [1 .. (width - 2)] do
        for y in [ 1 .. (height - 2) ] do
            yield matrix
                  |> List.where (inRange x y)
                  |> List.map (fun (_,_,c) -> c)
}

let matchMas (characters: char list) =
    match characters with
        | 'M' :: _ :: 'M' :: _ :: 'A' :: _ :: 'S' :: _ :: 'S' :: _ -> 1
        | 'S' :: _ :: 'S' :: _ :: 'A' :: _ :: 'M' :: _ :: 'M' :: _ -> 1
        | 'M' :: _ :: 'S' :: _ :: 'A' :: _ :: 'M' :: _ :: 'S' :: _ -> 1
        | 'S' :: _ :: 'M' :: _ :: 'A' :: _ :: 'S' :: _ :: 'M' :: _ -> 1
        | _ -> 0

let result2 = data |> group3x3 |> Seq.map matchMas |> Seq.sum
printfn $"Result 2: {result2}"

exit 0