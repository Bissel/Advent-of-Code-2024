#load @"../AdventOfCode/Utilities.fs"

open System.Collections.Generic
open AdventOfCode

type Stone = uint64

let withTiming func arg =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = func arg
    sw.Stop()
    (sw.Elapsed, res)

let data: Stone list = Utilities.getData "./day-11/input.txt" " "
                       |> Seq.head
                       |> Array.map uint64
                       |> Array.toList

let pow10: uint64 array = [|
    uint64 1
    uint64 10
    uint64 100
    uint64 1000
    uint64 10000
    uint64 100000
    uint64 1000000
    uint64 10000000
    uint64 100000000
    uint64 1000000000
    uint64 "10000000000"
    uint64 "100000000000"
    uint64 "1000000000000"
    uint64 "10000000000000"
    uint64 "100000000000000"
    uint64 "1000000000000000"
    uint64 "10000000000000000"
    uint64 "100000000000000000"
    uint64 "1000000000000000000"
    uint64 "10000000000000000000"
|] 

module Stone =
    let log10 (stone: uint64): int =
        (log10 (float stone)) |> int
        
    let hasEvenNumberOfDigits (stone: uint64): bool =
        (log10 stone) |> int |> (%) 2 |> (=) 0
        
    // As you observe them for a while, you find that the stones have a consistent behavior. Every time you blink, the stones each simultaneously change according to the first applicable rule in this list:

    // If the stone is engraved with the number 0,
    //   -> it is replaced by a stone engraved with the number 1.
    // If the stone is engraved with a number that has an even number of digits,
    //   -> it is replaced by two stones. The left half of the digits are engraved on the new left stone,
    //      and the right half of the digits are engraved on the new right stone.
    //     (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
    // If none of the other rules apply,
    //   -> the stone is replaced by a new stone;
    //      the old stone's number multiplied by 2024 is engraved on the new stone.
    let applyRules (stone: Stone): Stone list =
        if stone = (uint64 0) then
            [uint64 1]
        else
            let log = log10 stone
            if log % 2 = 1 then
                let splitPoint =  pow10[ ((log + 1) / 2) ]
                [(stone / splitPoint); (stone % splitPoint)]
            else
                [(stone * (uint64 2024))]
        
        // match stone with
        //     | 0 -> [(uint64)1]
        //     | s when hasEvenNumberOfDigits s -> Seq.splitInto 2 s
        //                                         |> Seq.map System.String
        //                                         |> Seq.map uint64
        //                                         |> Seq.toList
        //     | s -> [(s * (uint64 2024))]

let rec applyRules (steps: int) (stones: Stone list): Stone list =
    if steps = 0 then
        stones
    else
        stones
        |> List.map Stone.applyRules
        |> List.concat
        |> (applyRules (steps - 1))
    

let (time, afterBlinking25) = withTiming (applyRules 25) data

printfn $"{time}"

let result1 = Seq.length afterBlinking25

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let maxStep = 75
let cacheSize = uint64 10000
let mutable cache = Array2D.init maxStep (int cacheSize) (fun _ _ -> uint64 0)    
  
let rec applyRules2 (steps: int) (stone: Stone): uint64 =
    if steps = 0 then
        uint64 1
    else
        if stone > cacheSize then
            Stone.applyRules stone |> Seq.map (applyRules2 (steps - 1)) |> Seq.sum
        else
            let cached = cache[steps - 1, int stone]
            if cached > uint64 0 then
                cached
            else
                let res = Stone.applyRules stone |> Seq.map (applyRules2 (steps - 1)) |> Seq.sum
                cache[steps - 1, (int stone)] <- res
                res
               
let time2, afterBlinking75 = withTiming (fun a -> Seq.map (applyRules2 maxStep) a |> Seq.toList) data
             
printfn $"{time2}"
                      
let result2 = Seq.sum afterBlinking75
printfn $"Result 2: {result2}"

exit 0