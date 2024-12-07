#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

let data = Utilities.getData "./day-07/input.txt" " "
           |> Seq.map (fun row -> (
               Seq.head row |> String.filter ((<>) ':') |> uint64,
               Seq.tail row |> Seq.map uint64)
           )

let debugRow (s: uint64, num: uint64 seq) = $"{s}: {num |> Seq.map string |> Utilities.debugConcat }"

let checkEquation (operators: (uint64 -> uint64 -> uint64) list) (result: uint64, num: uint64 seq) =
    let rec _eval (num: uint64 list) =
        match num with
            | [] -> [uint64 0] |> List.toSeq
            | [a] -> [a] |> List.toSeq
            | b :: tail ->
                let pre = _eval tail
                seq {
                    for op in operators do
                        for eq in pre do
                            yield op eq b
                }
            
    let possibleEvaluations = num |> Seq.rev |> Seq.toList |> _eval
    
    possibleEvaluations |> Seq.exists ((=) result)

let equations = data
                   |> Seq.where (checkEquation [(+); (*)])

let result1 = equations |> Seq.map fst |> Seq.sum

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let concat (a: uint64) (b: uint64) = uint64 $"{a}{b}" 

let equations2 = data
                   |> Seq.where (checkEquation [(+); (*); concat])
let result2 = equations2 |> Seq.map fst |> Seq.sum
printfn $"Result 2: {result2}"

exit 0