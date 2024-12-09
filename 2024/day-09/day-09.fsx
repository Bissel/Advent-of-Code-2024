#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode
open Microsoft.FSharp.Core

type Id = int
type Length = int
type Disc = (Id*Length) list

module Disc =
    let rec fromData (i: Id) (data: Length list): Disc =
        match data with
            | [] -> []
            | [a] -> [(i, a)]
            | a :: b :: tail -> (i, a) :: (-1, b) :: fromData (i + 1) tail
            
    let toString (disc: Disc) =
        disc
            |> Seq.map (fun  (id, length) -> 
                let text = if id < 0 then "." else $"{id}"
                $"{(String.replicate length text)}"
            )
            |> String.concat ""
            
    let toRawString (disc: Disc) =
        disc
            |> Seq.map (fun  (id, length) -> 
                let text = if id < 0 then "." else $"{id}"
                $"({text}, {length})"
            )
            |> String.concat ""
            
    let _rev (data: Disc) = data |> Seq.rev |> Seq.toList
    
    let hightestId (disc: Disc): Id = disc |> List.map fst |> List.where ((<>) -1) |> List.last
    
    let blockIndex (id: Id) (disc: Disc) =
        disc |> Seq.findIndex (fun (_id, _) -> id = _id)
        
    let defragment (disc: Disc): Disc =
        let rec _setEmptySpaceRev (emptyLen: Length) (revData: Disc): Disc =
            if emptyLen <= 0 then
                revData
            else
                match revData with
                    | [] -> []
                    | [(-1, length)] -> [(-1, length)]
                    | [(id, length)] -> [(id, length)]
                    | (-1, len0) :: (-1, len1) :: tail -> (-1, len0 + len1) :: _setEmptySpaceRev emptyLen tail
                    | (-1, len) :: tail -> (-1, len) :: _setEmptySpaceRev emptyLen tail
                    | (id, blockLen) :: tail ->
                        if emptyLen = blockLen then
                            List.append tail [(id, blockLen)]
                        else if emptyLen < blockLen then
                            (id, blockLen - emptyLen) :: (List.append tail [(id, emptyLen)])  
                        else            
                            List.append (_setEmptySpaceRev (emptyLen - blockLen) tail) [(id, blockLen)]
        
        let rec _defrag (data: Disc) =
            match data with
                | [] -> []
                | [block] -> [block]
                | (-1, length) :: tail -> List.append
                                              (_defrag (_rev (_setEmptySpaceRev length (_rev tail))))
                                              [(-1, length)]
                | block :: tail -> block :: _defrag tail
        
        _defrag disc
    
        
    let defragment2 (disc: Disc) =
        let startId = (hightestId disc)
        
        let rec _move (id: Id) (data: Disc) =
            let index = blockIndex id data
            let length = snd data[index]
            
            let emptyBlockIndex = data |> List.tryFindIndex (fun (_id, len) -> _id = -1 && len >= length)
            
            if emptyBlockIndex.IsNone || emptyBlockIndex.Value > index then
                data
            else
                let emptyBlock = data[emptyBlockIndex.Value]
                let emptyBlockLength = snd emptyBlock
                
                let ab, c = List.splitAt index data |> fun (a,b) -> (a, List.tail b)
                let a, b =  List.splitAt emptyBlockIndex.Value ab |> fun (a,b) -> (a, List.tail b)
                
                let replacement =
                    if emptyBlockLength = length then
                        [(id, length)]
                    else
                        [(id, length); (-1, emptyBlockLength - length)]
                               
                List.concat [a; replacement; b; [(-1, length)]; c]
        
        let rec _defrag (id: Id) (data: Disc) =
            if id <= 0 then
                data
            else
                if id % 25 = 0 then
                    printfn $"{startId - id}"
                _defrag (id - 1) (_move id data)
        
        _defrag startId disc
        
    let checksum (disc: Disc) =
        disc
        |> fun s -> ((uint64 0, 0), s) ||> Seq.fold (fun col (id, len)  ->
            let _id = uint64 (if id < 0 then 0 else id)
            let _offset = uint64 (snd col)
            let sum = [0 .. (len - 1)]
                          |> Seq.map uint64
                          |> Seq.map (fun i -> _id * (_offset + i))
                          |> Seq.sum
                      
            (
                (fst col) + (if id = -1 then uint64 0 else sum),
                (snd col) + len
            )
        )
        |> fst

let disc =
    Utilities.readLines "./day-09/input.txt"
    |> Seq.head
    |> Seq.map string
    |> Seq.map int
    |> Seq.toList
    |> Disc.fromData 0
    
printfn $"{disc |> Disc.toString}"

// let defragmented = disc |> Disc.defragment 
//
// printfn $"{defragmented |> Disc.toString}"

let result1 = "6378826667552" // Disc.checksum defragmented

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let defragmented2 = disc |> Disc.defragment2

printfn $"{defragmented2 |> Disc.toString}"
let result2 = Disc.checksum defragmented2
printfn $"Result 2: {result2}"

exit 0