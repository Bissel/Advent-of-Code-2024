#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

type NumKeyPos =
    | Num of int
    | ButtonA
type Move =
    | DirL of int
    | DirU of int
    | DirD of int
    | DirR of int
    | PressA

module NumKeyPos =
    let toString (num: NumKeyPos) =
        match num with
            | Num n -> $"{n}"
            | ButtonA -> "A"

module Move =
    let toString (move: Move) =
        match move with
            | DirU n -> String.replicate n "^"
            | DirL n -> String.replicate n "<"
            | DirR n -> String.replicate n ">"
            | DirD n -> String.replicate n "v"
            | PressA -> "A"
            
    let Lenght (move: Move): uint64 =
        match move with
            | PressA -> uint64 1
            | DirU n -> uint64 n
            | DirD n -> uint64 n
            | DirL n -> uint64 n
            | DirR n -> uint64 n

module Numpad =
    
    // +---+---+---+
    // | 7 | 8 | 9 |
    // +---+---+---+
    // | 4 | 5 | 6 |
    // +---+---+---+
    // | 1 | 2 | 3 |
    // +---+---+---+
    //     | 0 | A |
    //     +---+---+
    let move (from: NumKeyPos) (number: NumKeyPos): Move list =
        let _sort (moves: Move list) : Move list =
            List.sortWith (fun a b ->
                match a with
                    | DirL _ -> match b with
                                | DirL _ -> 0
                                | _ -> 1 
                    | DirU _ -> match b with
                                | DirL _ -> -1
                                | DirU _ -> 0
                                | _ -> 1 
                    | DirD _ -> match b with
                                | DirL _ -> -1
                                | DirU _ -> -1
                                | DirD _ -> 0
                                | _ -> 1 
                    | DirR _ -> match b with
                                | DirR _ -> 0
                                | _ -> -1
                                
                    | _ -> 0
            ) moves
            
        let rec _move (from: NumKeyPos) (number: NumKeyPos) =
            match from with
                | ButtonA ->
                    match number with
                        | ButtonA -> []
                        | Num 1 -> [DirU 1; DirL 2]
                        | Num 4 -> [DirU 2; DirL 2]
                        | Num 7 -> [DirU 7; DirL 2]
                        | Num 2 -> [DirU 1; DirL 1]
                        | Num 5 -> [DirU 2; DirL 1]
                        | Num 8 -> [DirU 3; DirL 1]
                        | Num 0 ->         [DirL 1]
                        | Num _ -> [DirU 1] @ (_move (Num 3) number)
                | Num 0 ->
                    match number with
                        | Num 0 -> []
                        | ButtonA -> [DirR 1]
                        | Num 1 -> [DirU 1; DirL 1]
                        | Num 4 -> [DirU 2; DirL 1]
                        | Num 7 -> [DirU 7; DirL 1]
                        | Num 3 -> [DirU 1; DirR 1]
                        | Num 6 -> [DirU 2; DirR 1]
                        | Num 9 -> [DirU 3; DirR 1]
                        | Num _ -> [DirU 1] @ (_move (Num 2) number)
                | Num 2 ->
                    match number with
                        | Num 8 -> [DirU 2]
                        | Num 5 -> [DirU 1]
                        | Num 2 -> []
                        | Num 0 -> [DirD 1]
                        | Num n when n % 3 = 1 -> List.sort [DirL 1] @ (_move (Num 1) number)
                        | _                    -> List.sort [DirR 1] @ (_move (Num 3) number)
                | Num 5 ->
                    match number with
                        | Num 8 -> [DirU 1]
                        | Num 5 -> []
                        | Num 2 -> [DirD 1]
                        | Num 0 -> [DirD 2]
                        | Num n when n % 3 = 1 -> List.sort ([DirL 1] @ (_move (Num 4) number))
                        | _                    -> List.sort ([DirR 1] @ (_move (Num 6) number))
                | Num 8 ->
                    match number with
                        | Num 8 -> []
                        | Num 5 -> [DirD 1]
                        | Num 2 -> [DirD 2]
                        | Num 0 -> [DirD 3]
                        | Num n when n % 3 = 1 -> List.sort [DirL 1] @ (_move (Num 7) number)
                        | _                    -> List.sort [DirR 1] @ (_move (Num 9) number)
                
                
                | Num 1 ->
                    match number with
                        | Num 1 -> []
                        | Num 4 -> [DirU 1]
                        | Num 7 -> [DirU 2]
                        | _ -> List.sort [DirR 1] @ (_move (Num 2) number)
                | Num 4 ->
                    match number with
                        | Num 1 -> [DirD 1]
                        | Num 4 -> []
                        | Num 7 -> [DirU 1]
                        | _ -> List.sort [DirR 1] @ (_move (Num 5) number)
                | Num 7 ->
                    match number with
                        | Num 1 -> [DirD 2]
                        | Num 4 -> [DirD 1]
                        | Num 7 -> []
                        | _ -> List.sort [DirR 1] @ (_move (Num 8) number)
                        
                
                | Num 3 ->
                    match number with
                        | ButtonA -> [DirD 1]
                        | Num 3 -> []
                        | Num 6 -> [DirU 1]
                        | Num 9 -> [DirU 2]
                        | _ -> List.sort [DirL 1] @ (_move (Num 2) number)
                | Num 6 ->
                    match number with
                        | ButtonA -> [DirD 2]
                        | Num 3 -> [DirD 1]
                        | Num 6 -> []
                        | Num 9 -> [DirU 1]
                        | _ -> List.sort [DirL 1] @ (_move (Num 5) number)
                | Num 9 ->
                    match number with
                        | ButtonA -> [DirD 3]
                        | Num 3 -> [DirD 2]
                        | Num 6 -> [DirD 1]
                        | Num 9 -> []
                        | _ -> List.sort [DirL 1] @ (_move (Num 8) number)
                | _ -> failwith "hmmmm...."
        
        let rec merge (moves: Move list): Move list =
            match moves with
                | [] -> []
                | [a] -> [a]
                | DirU a :: DirU b :: tail -> DirU (a + b) :: merge tail 
                | DirD a :: DirD b :: tail -> DirD (a + b) :: merge tail 
                | DirL a :: DirL b :: tail -> DirL (a + b) :: merge tail 
                | DirR a :: DirR b :: tail -> DirR (a + b) :: merge tail
                | a :: tail -> a :: merge tail
            
        
        (_move from number |> merge) @ [PressA]
        
            
    let fromInput (input: string) : NumKeyPos list =
        input
        |> Seq.map (fun c ->
            match c with
                | 'A' -> ButtonA
                | n -> Num (n |> string |> int)
        ) 
        |> Seq.toList

//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+
module ArrowPad =
    let move (from: Move) (move: Move) =
        let press (n: int) = List.replicate n PressA
            
        match from with
            | PressA ->
                match move with
                    | DirU n -> [DirL 1] @ (press n)
                    | DirL n -> [DirD 1; DirL 2] @ (press n)
                    | DirR n -> [DirD 1] @ (press n)
                    | DirD n -> [DirL 1; DirD 1] @ (press n)
                    | _ -> [PressA]
            | DirU _ ->
                match move with
                    | DirU n -> (press n)
                    | DirL n -> [DirD 1; DirL 1] @ (press n)
                    | DirR n -> [DirD 1; DirR 1] @ (press n)
                    | DirD n -> [DirD 1] @ (press n)
                    | _ -> [DirR 1; PressA]
            | DirD _ ->
                match move with
                    | DirU n -> [DirU 1] @ (press n)
                    | DirL n -> [DirL 1] @ (press n)
                    | DirR n -> [DirR 1] @ (press n)
                    | DirD n -> (press n)
                    | _ -> [DirR 1; DirU 1; PressA]
            | DirL _ ->
                match move with
                    | DirU n -> [DirR 1; DirU 1;] @ (press n)
                    | DirL n -> (press n)
                    | DirR n -> [DirR 2] @ (press n)
                    | DirD n -> [DirR 1] @(press n)
                    | _ -> [DirR 2; DirU 1; PressA]
            | DirR _ ->
                match move with
                    | DirU n -> [DirL 1; DirU 1] @ (press n)
                    | DirL n -> [DirL 2] @ (press n)
                    | DirR n -> (press n)
                    | DirD n -> [DirL 1] @(press n)
                    | _ -> [DirU 1; PressA]

let data =
    Utilities.readLines "./day-21/input.txt"
    |> Seq.map Numpad.fromInput
    |> Seq.toList

let getKeyCombo (numKeyPosList : NumKeyPos list): Move list =
    (ButtonA :: numKeyPosList)
    |> List.pairwise
    |> List.map (fun (a,b) -> Numpad.move a b)
    |> List.concat
    
let getArrowCombo (moves : Move list): Move list =
    (PressA :: moves)
    |> List.pairwise
    |> List.map (fun (a,b) -> ArrowPad.move a b)
    |> List.concat

let rec doNMoves (n: int) (moves: Move list): Move list =
    if n <= 0 then
        moves
    else
        getArrowCombo (doNMoves (n - 1) moves) 
    

let doSteps (numberOfSteps: int) (numKeyPosList: NumKeyPos list): Move list =
    numKeyPosList |> getKeyCombo |> (doNMoves numberOfSteps)
    
let moveListToString (moves: Move list) : string =
    moves |> Seq.map Move.toString |> String.concat ""
    
    
// let test0 = [DirD 1; DirL 1]
// let test1 = [DirL 1; DirD 1]
//
// let t0 = test0 |> getArrowCombo |> getArrowCombo |> moveListToString
// let t1 = test1 |> getArrowCombo |> getArrowCombo |> moveListToString
//     
// printfn $"{t0.Length}: {t0}"
// printfn $"{t1.Length}: {t1}"
// exit 0

// let code0 = data[1]
// printfn $"""{code0 |> Seq.map NumKeyPos.toString |> String.concat "" }"""
// let step0 = code0 |> getKeyCombo
// let step1 = step0 |> getArrowCombo
// let step2 = step1 |> getArrowCombo
//
// let s0 = step0 |> Seq.map Move.toString |> String.concat ""
// let s1 = step1 |> Seq.map Move.toString |> String.concat ""
// let s2 = step2 |> Seq.map Move.toString |> String.concat ""
//     
// printfn $"{s0.Length}: {s0}"
// printfn $"{s1.Length}: {s1}"
// printfn $"{s2.Length}: {s2}"
// exit 0


let numberListToNumber (nums: NumKeyPos list): int =
    nums
    |> Seq.where ((<>) ButtonA)
    |> Seq.map NumKeyPos.toString
    |> String.concat ""
    |> int

let dataMoves = List.zip
                    (data |> List.map (doSteps 2) |> List.map moveListToString |> List.map _.Length)
                    (data |> List.map numberListToNumber)

printfn $"{dataMoves |> Seq.map Utilities.debugToString |> Utilities.debugConcat }"

let result1 = dataMoves |> Seq.map (fun (a,b) -> a * b) |> Seq.sum

printfn $"Result 1: {result1}"

// -------------------------------------------------- //
      
let rec chunkMoves (moves: Move list): Move list list =
    if moves.Length = 0 then
        []
    else 
        let index = List.tryFindIndex ((=) PressA) moves
        if index.IsNone then
            [moves]
        else 
            let front,back = List.splitAt (index.Value + 1) moves
            front :: (chunkMoves back)
       
let iterativeDoSteps (numberOfSteps: int) (numKeyPosList: NumKeyPos list): uint64 =
    let mutable subSequences: (Move list*uint64) list array = [|
        [];[];[];[];[];[];[];[];[];[];[];
        [];[];[];[];[];[];[];[];[];[];[];
        [];[];[];[];[];
    |]
        
    let rec comb (n: int) (moves: Move list): uint64 =
        if moves = [PressA] then
            uint64 1
        else if n = 0 then
            let cache = List.tryFind (fun a -> fst a = moves) subSequences[0]
            if cache.IsSome then
                snd cache.Value
            else
                let v = moves |> Seq.map Move.Lenght |> Seq.sum
                subSequences[0] <- (moves, v) :: subSequences[0]
                v
        else if n < subSequences.Length then
            let cache = List.tryFind (fun a -> fst a = moves) subSequences[n]
            if cache.IsSome then
                snd cache.Value
            else
                let v = chunkMoves moves |> Seq.map getArrowCombo |> Seq.map (comb (n - 1)) |> Seq.sum
                subSequences[n] <- (moves, v) :: subSequences[n]
                printfn $"""added to cache[{n}] {moves |> Seq.map Move.toString |> String.concat "" } : {v}"""
                v
        // else if n = 17 then
        //     let s = doNMoves n moves |> Seq.map Move.Lenght |> Seq.sum
        //     printfn $"{s}"
        //     s
        // if n = 0 then
        //     
        else
            chunkMoves moves |> Seq.map getArrowCombo |> Seq.map (comb (n - 1)) |> Seq.sum
            // getArrowCombo moves //|> chunkMoves |> Seq.map (comb (n - 1)) |> Seq.sum
            // |> (comb (n - 1))
    
    let res = getKeyCombo numKeyPosList |> chunkMoves |> Seq.map (comb numberOfSteps) |> Seq.sum
    printfn $"""{numKeyPosList |> Seq.map NumKeyPos.toString |> String.concat ""}: {res}"""
    res
    

let dataMoves2 = List.zip
                    (data |> List.map (iterativeDoSteps 25))
                    (data |> List.map numberListToNumber)

printfn $"{dataMoves2 |> Seq.map Utilities.debugToString |> Utilities.debugConcat }"

let result2 = dataMoves2 |> Seq.map (fun (a,b) -> a * (uint64 b)) |> Seq.sum
printfn $"Result 2: {result2}" // 299463372555238

exit 0