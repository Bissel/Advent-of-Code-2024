#load @"../AdventOfCode/Utilities.fs"

type Key  = (int*int*int*int*int)
type Lock = (int*int*int*int*int)


open System
open AdventOfCode

let lockFromInput (input: string list): Lock =
    let mutable a = [|[];[];[];[];[]|]
    
    for i in [1..6] do
        for x in [0..4] do
            a[x] <- a[x] @ [input[i][x]]
    (
        a[0] |> Seq.findIndex ((=) '.'),
        a[1] |> Seq.findIndex ((=) '.'),
        a[2] |> Seq.findIndex ((=) '.'),
        a[3] |> Seq.findIndex ((=) '.'),
        a[4] |> Seq.findIndex ((=) '.')
    )

let keyFromInput (input: string list): Key = lockFromInput (List.rev input)

let fromInput (input: string seq): (Key list)*(Lock list) =
    let mutable a: string list = []
    let mutable keys: Key list = []
    let mutable locks: Lock list = []
    
    for row in input do
        if row = "" then
            if a[0] = "....." then
                keys <- keyFromInput a :: keys
            else
                locks <- lockFromInput a :: locks
            
            a <- []
        else
            a <- a @ [row] 
        
    (keys, locks)
    
let keyFit ((k0, k1, k2, k3, k4): Key) ((l0, l1, l2, l3, l4): Lock): bool =
    k0 + l0 < 6 &&
    k1 + l1 < 6 &&
    k2 + l2 < 6 &&
    k3 + l3 < 6 &&
    k4 + l4 < 6
    
    
let keys, locks =
    Utilities.readLines "./day-25/input.txt"
    |> fromInput


// for key in keys do
//     printfn $"K: {key}"
// for lock in locks do
//     printfn $"L: {lock}"

let keyFitCount =
    Seq.allPairs keys locks
    |> Seq.where (fun (key, lock) -> keyFit key lock)
    |> Seq.length


let result1 = keyFitCount

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0