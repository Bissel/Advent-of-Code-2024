#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

let data = Utilities.readLines("./day-05/input.txt")
           |> Seq.toArray

let splitPoint = data |> Seq.indexed |> Seq.find (fun (_, line) -> String.length line = 0) |> fst

let rules = data
            |> Seq.take splitPoint
            |> Seq.map (fun line -> line.Split "|")
            |> Seq.map (fun rule -> (int rule[0], int rule[1]) )
            |> Seq.toList

let updates = data
            |> Seq.skip (splitPoint + 1)
            |> Seq.map (fun line -> line.Split ",")
            |> Seq.map (fun rule -> rule |> Seq.map int |> Seq.toList)
            |> Seq.toList

let rec getMiddleNumber (numbers: int list) =
    match numbers with
      | [] -> 0
      | [m] -> m
      | [m; _]  -> m
      | _ :: tail -> getMiddleNumber ( List.rev (List.tail (List.rev tail)) )

let getResult update = update |> Seq.map getMiddleNumber |> Seq.sum
      
let getRulesForUpdate (update: int seq) = rules |> Seq.where  (fun (a,b) -> Seq.contains a update && Seq.contains b update)
let getNextRules (page: int) (rulesForUpdate: (int*int) seq) = rulesForUpdate |> Seq.where (fun (p,_) -> p = page) |> Seq.map snd
let getPrevRules (page: int) (rulesForUpdate: (int*int) seq) = rulesForUpdate |> Seq.where (fun (_,p) -> p = page) |> Seq.map fst

let isPageCorrect (page: int) (rulesForUpdate: (int*int) seq) (nextPages: int list) =
    let nextRules = getNextRules page rulesForUpdate
    
    let next = if Seq.length nextRules > 0
                   then Seq.forall (fun rule -> Seq.contains rule nextPages) nextRules
                   else true
                   
    next
    
let isPageCorrectWithPrev (page: int) (rulesForUpdate: (int*int) seq) (nextPages: int list) =
    let prevRules = getPrevRules page rulesForUpdate
    let nextRules = getNextRules page rulesForUpdate
    
    let prev = if Seq.length prevRules > 0
                   then not (Seq.exists (fun rule -> Seq.contains rule nextPages) prevRules)
                   else true
                   
    let next = if Seq.length nextRules > 0
                   then Seq.forall (fun rule -> Seq.contains rule nextPages) nextRules
                   else true
                   
    prev && next 

let isUpdateCorrect (update: int list) =
    let rulesForUpdate = getRulesForUpdate update
    // printfn $"update: {update |> Seq.map string |> Utilities.debugConcat}"
    // printfn $"rulesForUpdate: {rulesForUpdate |> Seq.map Utilities.debugToString |> Utilities.debugConcat}"
    // printfn ""
    let rec _isUpdateCorrect (update: int list)
        = match update with
            | [] -> true
            | head :: tail -> if isPageCorrect head rulesForUpdate tail 
                                then _isUpdateCorrect tail
                                else false
                                
    _isUpdateCorrect update
                            
let correctUpdates, incorrectUpdates = updates |> Seq.splitBy isUpdateCorrect

let result1 = getResult correctUpdates
              

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let correctUpdate (update: int list) =
    let rulesForUpdate = getRulesForUpdate update
    
    // printfn $"update: {update |> Seq.map string |> Utilities.debugConcat}"
    // printfn $"rulesForUpdate: {rulesForUpdate |> Seq.map Utilities.debugToString |> Utilities.debugConcat}"
    
    let rec _movePage (prevRules: int seq) (page: int) (tail: int list) =
        match tail with
            | [] -> [page]
            | nextPage :: other -> if Seq.exists (fun rule -> Seq.contains rule tail) prevRules
                                    then (nextPage :: (_movePage prevRules page other))
                                    else (page :: nextPage :: other)
    
    let _correctPage (page: int) (tail: int list) =
        let prevRules = getPrevRules page rulesForUpdate
        _movePage prevRules page tail
    
    let rec _correctUpdate (update: int list) =
        match update with
            | [] -> []
            | [a] -> [a]
            | page :: tail -> if (isPageCorrectWithPrev page rulesForUpdate tail)
                                then (page :: (_correctUpdate tail) )
                                else (_correctUpdate (_correctPage page tail))
                
    _correctUpdate update


let corrected = incorrectUpdates
                |> Seq.map correctUpdate

let result2 = getResult corrected
printfn $"Result 2: {result2}"

exit 0