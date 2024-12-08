#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode
open Microsoft.FSharp.Core

module Vector =
    let sub (a: int*int) (b: int*int) = (fst a - fst b, snd a - snd b)
    let add (a: int*int) (b: int*int) = (fst a + fst b, snd a + snd b)
    let scale (factor: int) (vector: int*int) = (fst vector * factor, snd vector * factor)
    
module Pair =
    let unequal (a: int*int, b: int*int) = a <> b
    
    let sort (a: int*int, b: int*int) = if a < b then (a,b) else (b,a) 
 
let data = Utilities.readLines("./day-08/input.txt") |> Utilities.toMatrix

let width, height = data |> Seq.rev |> Seq.head |> fun (x,y,_) -> (x + 1, y + 1)

let uniqueFrequencies =
    data
    |> Seq.where (fun (_,_,c) -> c <> '.')
    |> Seq.groupBy (fun (_,_,c) -> c)
    |> Seq.map (fun (key, points) -> (key, points |> Seq.map (fun (x,y,_) -> (x,y))))


// In particular, an antinode occurs at any point that is
// - perfectly in line with two antennas of the same frequency
// - but only when one of the antennas is twice as far away as the other.
// This means that for any pair of antennas with the same frequency,
//  - there are two antinodes,
//  - one on either side of them.
let constructDualAntiNodes (a: int*int, b: int*int) = seq {
        // a = ( 6,  5)
        // b = ( 8,  8)
        // c = ( 2,  3)
        let vector = Vector.sub b a
        yield Vector.sub a vector
        yield Vector.add b vector
    }

let getAntiNodes constructAntiNodes (points: (int*int) seq)=
    Seq.allPairs points points
        |> Seq.where Pair.unequal
        |> Seq.map Pair.sort
        |> Seq.distinct
        |> Seq.map constructAntiNodes
        |> Seq.concat
        |> Seq.where (fun (x,y) -> x >= 0 && x < width && y >= 0 && y < height )

// for freq in uniqueFrequencies do
//     printfn $"{fst freq} {snd freq |> Seq.map Utilities.debugToString |> Utilities.debugConcat }"
//     printfn $"{getAntiNodes (snd freq) |> Seq.map Utilities.debugToString |> Utilities.debugConcat}"

// printfn $"{width} {height}"

// O -> (6,0) (11,0) (3,1) (10,2) (
// A -> (4,2) 

//   0 1 2 3 4 5 6 7 8 9 A B
// 0 . . . . . . # . . . . #
// 1 . . . # . . . . 0 . . .
// 2 . . . . # 0 . . . . # .
// 3 . . # . . . . 0 . . . .
// 4 . . . . 0 . . . . # . .
// 5 . # . . . . A . . . . .
// 6 . . . # . . . . . . . .
// 7 # . . . . . . # . . . .
// 8 . . . . . . . . A . . .
// 9 . . . . . . . . . A . .
// A . . . . . . . . . . # .
// B . . . . . . . . . . # .

let getAntiNodesForTowers constructAntiNodes (towers: (char * (int*int) seq) seq) =
     towers
         |> Seq.map snd
         |> Seq.where (fun s -> 1 < (Seq.length s))
         |> Seq.map (getAntiNodes constructAntiNodes)
         |> Seq.concat
         |> Seq.distinct
    
let antinodes = getAntiNodesForTowers constructDualAntiNodes uniqueFrequencies

let result1 = Seq.length antinodes

printfn $"Result 1: {result1}"

// -------------------------------------------------- //


let constructAllAntiNodes (a: int*int, b: int*int) = seq {
        // a = ( 6,  5)
        // b = ( 8,  8)
        // c = ( 2,  3)
        let vector = Vector.sub b a
        
        let maxSteps = (max (width / fst vector) (height / snd vector)) + 1
        
        for i in [0..maxSteps] do
            yield Vector.sub a (Vector.scale i vector)
        
        for i in [0..maxSteps] do
            yield Vector.add b (Vector.scale i vector)
    }

// for freq in uniqueFrequencies do
//     printfn $"{fst freq} {snd freq |> Seq.map Utilities.debugToString |> Utilities.debugConcat }"
//     let an = getAntiNodes constructAllAntiNodes (snd freq)
//                 |> Seq.distinct
//     printfn $"{an |> Seq.map Utilities.debugToString |> Utilities.debugConcat}"

//   0 1 2 3 4 5 6 7 8 9 A B
// 0 # # . . . . # . . . . #
// 1 . # . # . . . . 0 . . .
// 2 . . # . # 0 . . . . # .
// 3 . . # # . . . 0 . . . .
// 4 . . . . 0 . . . . # . .
// 5 . # . . . # A . . . . #
// 6 . . . # . . # . . . . .
// 7 # . . . . # . # . . . .
// 8 . . # . . . . . A . . .
// 9 . . . . # . . . . A . .
// A . # . . . . . . . . # .
// B . . . # . . . . . . # #


let antinodes2 = getAntiNodesForTowers constructAllAntiNodes uniqueFrequencies
let result2 = Seq.length antinodes2
printfn $"Result 2: {result2}"

exit 0