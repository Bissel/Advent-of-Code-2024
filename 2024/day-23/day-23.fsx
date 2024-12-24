#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

let fromInput (input: (string*string) seq) =
    (Seq.allPairs input (Seq.allPairs input input))
    |> Seq.map (fun (a,(b,c)) -> (a, b, c))
    |> Seq.where (fun (a,b,c) -> a <> b || a <> c || b <> c)
    |> Seq.where (fun ((a, b), (c, d), (e, f)) ->
        // a b        c d       e f
        // a b        c d       f e
        // a b        d c       e f
        // a b        d c       f e
        // b a        c d       e f
        // b a        c d       f e
        // b a        d c       e f
        // b a        d c       f e
        b = c && d = e && f = a || 
        b = c && d = f && e = a || 
        b = d && c = e && f = a || 
        b = d && c = f && e = a || 
        a = c && d = e && f = b ||
        a = c && d = f && e = b ||
        a = d && c = e && f = b ||
        a = d && c = f && e = b
    )
    |> Seq.map (fun ((a, b), (c, d), (e, f)) -> [|a; b; c; d; e; f|])
    |> Seq.map (fun a -> a |> Array.sort |> Array.distinct) 
    |> Seq.map (fun a -> (a[0],a[1],a[2]))
    |> Seq.sort
    |> Seq.distinct

let data =
    Utilities.getData "./day-23/input.txt" "-"
    |> Seq.map Array.sort
    |> Seq.map (fun a -> (a[0], a[1]))
    |> fromInput
    
let filterHistorian (search) (a: string, b: string, c: string) =
    search a || search b || search c

let filtered = data
               |> Seq.where (filterHistorian (fun a -> ((=) 't') (Seq.head a)))     

let result1 = filtered |> Seq.length

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0