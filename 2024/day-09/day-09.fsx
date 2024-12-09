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
    
    let defragment (disc: Disc): Disc =
        let _rev (data: Disc) = data |> Seq.rev |> Seq.toList
        
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
        
        // let rec _setEmptySpace (emptyLen: Length) (data: Disc): Disc =
        //     if emptyLen <= 0 || data.Length <= 1 then
        //         data
        //     else
        //         let id, blockLen = List.last data
        //         
        //         if id = -1 then
        //             List.append (_setEmptySpace emptyLen (List.truncate (data.Length - 1) data) ) [(-1, blockLen)]
        //         else
        //             
        //         
        //         match revData with
        //             | [] -> []
        //             | [(-1, _)] -> []
        //             | [(id, length)] -> [(id, length)]
        //             | (id, blockLen) :: tail ->
        //                 if emptyLen = blockLen then
        //                     List.append tail [(id, blockLen)]
        //                 else if emptyLen < blockLen then
        //                     (id, blockLen - emptyLen) :: (List.append tail [(id, emptyLen)])  
        //                 else            
        //                     List.append (_setEmptySpace (emptyLen - blockLen) tail) [(id, blockLen)]
        
        let rec _defrag (data: Disc) =
            match data with
                | [] -> []
                | [block] -> [block]
                | (-1, length) :: tail -> List.append
                                              (_defrag (_rev (_setEmptySpaceRev length (_rev tail))))
                                              [(-1, length)]
                | block :: tail -> block :: _defrag tail
        
        _defrag disc
        
    let checksum (disc: Disc) =
        disc
        |> Seq.where (fun t -> (<>) -1 (fst t))
        |> fun s -> ((uint64 0, 0), s) ||> Seq.fold (fun col (id, len)  ->
            let _id = uint64 id
            let _offset = uint64 (snd col)
            let sum = [0 .. (len - 1)]
                      |> Seq.map uint64
                      |> Seq.map (fun i -> _id * (_offset + i))
                      |> Seq.sum
                      
            ( (fst col) + sum, (snd col) + len )
        )
        |> fst

let disc =
    Utilities.readLines "./day-09/input.txt"
    |> Seq.head
    |> Seq.map string
    |> Seq.map int
    |> Seq.toList
    |> Disc.fromData 0
    
// printfn $"{disc |> Disc.toRawString}"
printfn $"{disc |> Disc.toString}"

let defragmented = disc |> Disc.defragment 

printfn $"{defragmented |> Disc.toString}"

let result1 = Disc.checksum defragmented

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0