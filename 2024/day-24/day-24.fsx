#load @"../AdventOfCode/Utilities.fs"

open System
open AdventOfCode

type Gate = ((bool -> bool -> bool) * string * string)

let valueFromInput (input: String) =
    input.Split(": ")  |> (fun a -> (a[0], a[1] = "1"))
 
let gateFromInput (input: String): string*Gate =
    let gate,result = input.Split(" -> ") |> (fun a -> (a[0],a[1]))
    let a,op,b = gate.Split(" ") |> (fun a -> (a[0],a[1],a[2]))
    
    let operand = match op with
                    | "AND" -> (&&)
                    | "OR"  -> (||)
                    | "XOR" -> (<>)
                    | _ -> failwith "not supported"
    
    (result, (operand, a, b))

let parseInput (input: String seq) =
    let values, gates = input |> Seq.toList |> List.splitAt (input |> Seq.findIndex ((=) ""))
    
    (
        values |> Seq.map valueFromInput |> Map,
        gates |> Seq.tail |> Seq.map gateFromInput |> Map
    )

let values, gates =
    Utilities.readLines "./day-24/input.txt"
    |> parseInput

let solve (values: Map<string,bool>) (gates: Map<string,Gate>) =
    let mutable valueMap: Map<string, bool> = values
    let mutable gatesToSolve: string list = gates |> Map.filter (fun k _ -> Seq.head k = 'z') |> Map.keys |> Seq.toList
    
    let getValue key = valueMap |> Map.tryFind key 
    
    while gatesToSolve.Length > 0 do
        printfn $"{gatesToSolve.Length}"
        let gateKey = List.head gatesToSolve
        
        if getValue gateKey |> _.IsSome then
            gatesToSolve <- List.tail gatesToSolve
        else
            let op, aKey, bKey = gates |> Map.find gateKey
            
            let a = getValue aKey 
            let b = getValue bKey
            
            if a.IsSome && b.IsSome then
                valueMap <- valueMap |> Map.add gateKey (op a.Value b.Value)
                gatesToSolve <- List.tail gatesToSolve
            else
                gatesToSolve <- (if a.IsNone then [aKey] else [])
                              @ (if b.IsNone then [bKey] else [])
                              @ [gateKey]
                              @ (List.tail gatesToSolve)
            
    valueMap |> Map.filter (fun k _ -> Seq.head k = 'z')
    
let toInt (from: bool seq): uint64 =
    let mutable a = uint64 0
    
    for b in from do
        a <- (a <<< 1) + uint64 (if b then 1 else 0)
        
    a
    
let solved = solve values gates

let result1 = solved |> Map.values |> Seq.rev |> toInt

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0