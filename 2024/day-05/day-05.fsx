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
      
      
let getRulesForUpdate (update: int seq) = rules |> Seq.where  (fun (a,b) -> Seq.contains a update && Seq.contains b update)

let getNextRules (page: int) (rulesForUpdate: (int*int) seq) = rulesForUpdate |> Seq.where (fun (p,_) -> p = page) |> Seq.map snd
let getPrevRules (page: int) (rulesForUpdate: (int*int) seq) = rulesForUpdate |> Seq.where (fun (_,p) -> p = page) |> Seq.map fst

let isPageCorrect (page: int) (rulesForUpdate: (int*int) seq) (prevPages: int list) (nextPages: int list) =
    let prevRules = getPrevRules page rulesForUpdate
    let nextRules = getNextRules page rulesForUpdate
    
    let prev = if Seq.length prevRules > 0
                   then prevRules |> Seq.forall (fun rule -> Seq.contains rule prevPages) 
                   else true
                   
    let next = if Seq.length nextRules > 0
                   then nextRules |> Seq.forall (fun rule -> Seq.contains rule nextPages) 
                   else true
                   
    prev && next 

let isUpdateCorrect (update: int list) =
    let rulesForUpdate = getRulesForUpdate update
    let rec _isUpdateCorrect (update: int list) (headList: int list)
        = match update with
            | [] -> true
            | head :: tail -> if isPageCorrect head rulesForUpdate headList tail 
                                then _isUpdateCorrect tail (List.concat [headList; [head]])
                                else false
                                
    _isUpdateCorrect update []
                            
let correctUpdates = updates |> Seq.where isUpdateCorrect

let c = correctUpdates |> Seq.map (fun a -> Seq.map string a |> Utilities.debugConcat)

let result1 = correctUpdates
              |> Seq.map getMiddleNumber
              |> Seq.sum

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let incorrectUpdates = updates |> Seq.where (fun a -> not (isUpdateCorrect a))




let result2 = "-"
printfn $"Result 2: {result2}"

exit 0