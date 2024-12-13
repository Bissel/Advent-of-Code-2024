#load @"../AdventOfCode/Utilities.fs"

open System.Drawing
open AdventOfCode

type Point = int*int
type Vector = int*int

module Vector =
    let scale (factor: int) (v: Vector) : Vector =
        (fst v * factor, snd v * factor)
        
    let add (a: Vector) (b: Vector) : Vector =
        (fst a + fst b, snd a + snd b)
        
    let div (a: Vector) (b: Vector) : Vector =
        if fst b = 0 || snd b = 0 then
            (0,0)
        else
            (fst a / fst b, snd a / snd b)
            
    let min (v: Vector) = min (fst v) (snd v)
    let max (v: Vector) = max (fst v) (snd v)

    let fromPoint (p: Point): Vector = p
    
    
// Button A Press
// Button B Press
// Prize
type SlotMachine = Vector*Vector*Point

module SlotMachine =
    let toString ((a,b,p): SlotMachine) =
        $"{{ {a}, {b} | {p} }}"

let costA = 3
let costB = 1

let buttonA ((a,_,_) : SlotMachine) : Vector = a
let buttonB ((_,b,_) : SlotMachine) : Vector = b
let prize ((_,_,p) : SlotMachine) : Point = p

let rec dataToSlotMachine (lines : string list) : SlotMachine list =
    let allowedChars = "0123456789,"
    let toSlotMachine (a:string) (b:string) (p:string) =
        let toVector s = s |> Seq.where (fun c -> allowedChars |> Seq.exists ((=)c) )
                           |> Seq.map string
                           |> String.concat ""
                           |> fun (a: System.String) -> a.Split ","
                           |> fun split -> (
                                split[0] |> int,
                                split[1] |> int
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
    Utilities.readLines "./day-13/input.txt"
    |> Seq.toList
    |> dataToSlotMachine

let price (buttonAPresses: int, buttonBPresses: int) =
    buttonAPresses * costA + buttonBPresses * costB

let getComposition (a: Vector, b: Vector, p: Point): (int*int) option =
    let axisCombination (sel: int*int -> int) = seq {
        let xp = sel p
        let xa = sel a
        let xb = sel b
        let xaMax = xp / xa
        let xbMax = xp / xb
        
        for xbScale in [0 .. xbMax + 1] do
            for xaScale in [0 .. xaMax + 1] do
                if xa * xaScale + xb * xbScale = xp then
                    yield (xaScale, xbScale)
    }
    
    let xAxisCombinations = axisCombination fst |> Seq.toList
    let yAxisCombinations = axisCombination snd
    
    if xAxisCombinations.Length = 0 then
        None
    else
        Seq.allPairs xAxisCombinations yAxisCombinations
            |> Seq.where (fun (a,b) -> a = b)
            |> Seq.map fst
            |> Seq.sortBy fst
            |> Seq.tryHead  
    
let compositions =
    data
    |> Seq.map getComposition
    |> Seq.tapi (fun i c ->
        printfn $"{i}: {if c.IsNone then (-1, -1) else c.Value }"    
    )
    |> Seq.where _.IsSome
    |> Seq.map _.Value

let coins = compositions |> Seq.map price

let result1 = coins |> Seq.sum

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0