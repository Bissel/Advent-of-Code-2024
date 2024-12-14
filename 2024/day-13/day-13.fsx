#load @"../AdventOfCode/Utilities.fs"
#load @"../AdventOfCode/Vector.fs"

open AdventOfCode

type Point = int64*int64

module Vector64 = 
    let min (v: Vector64) = min (fst v) (snd v)
    let max (v: Vector64) = max (fst v) (snd v)
    
    
// Button A Press
// Button B Press
// Prize
type SlotMachine = Vector64*Vector64*Point

module SlotMachine =
    let toString ((a,b,p): SlotMachine) =
        $"{{ {a}, {b} | {p} }}"

let costA = int64 3
let costB = int64 1

let buttonA ((a,_,_) : SlotMachine) : Vector64 = a
let buttonB ((_,b,_) : SlotMachine) : Vector64 = b
let prize ((_,_,p) : SlotMachine) : Point = p

let rec dataToSlotMachine (lines : string list) : SlotMachine list =
    let allowedChars = "0123456789,"
    let toSlotMachine (a:string) (b:string) (p:string) =
        let toVector s = s |> Seq.where (fun c -> allowedChars |> Seq.exists ((=)c) )
                           |> Seq.map string
                           |> String.concat ""
                           |> fun (a: System.String) -> a.Split ","
                           |> fun split -> (
                                split[0] |> int64,
                                split[1] |> int64
                            )
        (
            toVector a,
            toVector b,
            toVector p
        )
        
    match lines with
        | [] -> []
        | [_] -> []
        | [_;_] -> []
        | [_;_;_] -> []
        | a :: b :: p :: _ :: tail -> (toSlotMachine a b p) :: dataToSlotMachine tail

let data =
    Utilities.readLines "./day-13/example.txt"
    |> Seq.toList
    |> dataToSlotMachine

let price (buttonAPresses: int64, buttonBPresses: int64) =
    buttonAPresses * costA + buttonBPresses * costB

let getComposition (a: Vector64, b: Vector64, p: Point): (int64*int64) option =
    let axisCombination (sel: int64*int64 -> int64) = seq {
        let xp = sel p
        let xa = sel a
        let xb = sel b
        let xaMax = xp / xa
        let xbMax = xp / xb
        
        for xbScale in [(int64 0) .. xbMax + (int64 1)] do
            for xaScale in [(int64 0) .. xaMax + (int64 1)] do
                if xa * xaScale + xb * xbScale = xp then
                    yield (xaScale, xbScale)
    }
    
    let xAxisCombinations = axisCombination fst
    let yAxisCombinations = axisCombination snd
    
    Seq.allPairs xAxisCombinations yAxisCombinations
        |> Seq.where (fun (a,b) -> a = b)
        |> Seq.map fst
        |> Seq.sortBy fst
        |> Seq.tryHead  

let rec f (n: int64) x a =
    if x > (int64 100000) then
        a
    else if x = n then
        x::a
    elif n % x = 0 then 
        f (n/x) x (x::a)
    else
        f n (x + (int64 1)) a
let factorise (n: int64) = f n (int64 2) []   

let getComposition2 (a: Vector64, b: Vector64, p: Point): (int64*int64) option =
    let axisCombination = seq {
        let xp0 = fst p
        let xp1 = snd p
        let xa0 = fst a
        let xa1 = snd a
        let xb0 = fst b
        let xb1 = snd b
        let xaMax = max (xp0 / xa0) (xp1 / xa1)
        let xbMax = max (xp0 / xb0) (xp1 / xb1)
        
        let xa0Factors = factorise xa0
        let xa1Factors = factorise xa1
        let xb0Factors = factorise xb0
        let xb1Factors = factorise xb1
        
        for xbScale in [(int64 0) .. xbMax + (int64 1)] do
            for xaScale in [(int64 0) .. xaMax + (int64 1)] do
                if xa0 * xaScale + xb0 * xbScale = xp0 &&
                   xa1 * xaScale + xb1 * xbScale = xp1 then
                    yield (xaScale, xbScale)
    }
    
    axisCombination
        |> Seq.tryHead  
    
let compositions =
    data
    |> Seq.map getComposition2
    // |> Seq.tapi (fun i c ->
    //     printfn $"{i}: {if c.IsNone then (int64 -1, int64 -1) else c.Value }"    
    // )
    |> Seq.where _.IsSome
    |> Seq.map _.Value

let result1 = compositions |> Seq.map price |> Seq.sum

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let addToPrize = int64 "10000000000000"


let getComposition3 (a: Vector64, b: Vector64, p: Point): (int64*int64) option =
    // let axisCombination = seq {
    let p0 = fst p
    let p1 = snd p
    let a0 = fst a
    let a1 = snd a
    let b0 = fst b
    let b1 = snd b
    // let aMax = max (p0 / a0) (p1 / a1)
    // let bMax = max (p0 / b0) (p1 / b1)
    
    let p0Factors = factorise p0 |> List.distinct
    let p1Factors = factorise p1 |> List.distinct
    
    printfn $"{p0}: {p0Factors}"
    printfn $"{p1}: {p1Factors}"
    printfn $""
    
    let s = (Seq.allPairs p0Factors p1Factors)
                |> Seq.where (fun (f0, f1) -> f0 = f1)
                |> Seq.map fst
        
    s |> Seq.map (fun scale -> (scale, getComposition2 (a, b, Vector64.div p (scale,scale))) )
      |> Seq.where (fun (_, v) -> v.IsSome)
      |> Seq.map (fun (s,v) -> Vector64.scale s v.Value)
      |> Seq.tryHead
    //     |> Seq.where (fun (p0, p1) ->
    //         p0 % a0 = 0 && p0 % b0 = 0 &&
    //         p1 % a1 = 0 && p1 % b1 = 0 &&
    //         p0 / a0 = p1 / a1 &&
    //         p0 / b0 = p1 / b1
    //     )
    //     |> Seq.tryHead  
    //     
        // for xbScale in [(int64 0) .. xbMax + (int64 1)] do
        //     for xaScale in [(int64 0) .. xaMax + (int64 1)] do
        //         if xa0 * xaScale + xb0 * xbScale = xp0 &&
        //            xa1 * xaScale + xb1 * xbScale = xp1 then
        //             yield (xaScale, xbScale)
    // }
    
    // axisCombination
    //     |> Seq.tryHead  

let getSubComposition (a: Vector64, b: Vector64, p: Point): (int64*int64) option =
    // let baseCompositions = [|
    //     int64 "1000",
    //     int64 "10000",
    //     int64 "100000",
    //     int64 "1000000",
    //     int64 "10000000",
    //     int64 "100000000",
    //     int64 "1000000000",
    //     int64 "10000000000",
    //     int64 "100000000000",
    //     int64 "1000000000000",
    //     int64 "10000000000000"
    // |]
    // getComposition2 (a, b, (10, 10))
    getComposition3 (a, b, p)
    
    

let compositions2 =
    data
    |> Seq.map (fun (a,b,p) -> (a,b, Vector64.addi p addToPrize))
    |> Seq.map getSubComposition
    |> Seq.tapi (fun i c ->
        printfn $"{i}: {if c.IsNone then (int64 -1, int64 -1) else c.Value }"    
    )
    |> Seq.where _.IsSome
    |> Seq.map _.Value

let result2 = compositions2 |> Seq.map price |> Seq.sum

printfn $"Result 2: {result2}"

exit 0