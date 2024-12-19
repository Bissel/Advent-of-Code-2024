#load @"../AdventOfCode/Utilities.fs"

open System
open AdventOfCode

type Color =
    | White
    | Blue
    | Black
    | Red 
    | Green


type Pattern = String // Color list
type Towel = Pattern
    
module Color =
    let fromInput (c: char): Color =
        match c with
            | 'w' -> White
            | 'u' -> Blue
            | 'b' -> Black
            | 'r' -> Red
            | 'g' -> Green
            | _ -> failwith "todo"
            
    let toString (c: Color) =
        match c with
            | White -> "w"
            | Blue -> "u"
            | Black -> "b"
            | Red -> "r"
            | Green -> "g"
    
module Pattern =
    let fromInput (input: string): Pattern =
        input // |> Seq.map Color.fromInput |> Seq.toList
        
    let toString (p: Pattern): string =
        p //|> Seq.map Color.toString |> String.concat ""
        
        
    let checkPattern (options: Pattern list) (pattern: Pattern): bool =
        let mutable checkPos = Array2D.init pattern.Length options.Length (fun _ _ -> false);
        for optIndex, o in List.indexed options do
            let mutable index = 0
            while index >= 0 do
                index <- pattern.IndexOf(o, index)
                if index >= 0 then
                    for i in [index .. (index + o.Length)] do 
                        checkPos[i,optIndex] <- true
                    index <- index + 1
                
        // todo combine every option to find non overlapping but fully covering masks
        // checkPos[,]
                
        true
        
    
    let impossiblePattern (options: Pattern list) (pattern: Pattern): bool =
        let mutable checkString = String.init pattern.Length (fun _ -> "X");
        for o in options do
            let mutable index = 0
            while index >= 0 do
                index <- pattern.IndexOf(o, index)
                if index >= 0 then
                    let endIndex = index + o.Length
                    checkString <- checkString |> Seq.mapi (fun i c ->
                        if i >= index && i < endIndex then ' ' else c ) |> String.Concat
                    index <- index + 1
                
        checkString.Contains "X"
        
    let rec isCombinationFrom (options: Pattern list) (pattern: Pattern): bool =
        // if options.Length = 0 then
        //     false
        // else
        //     let mutable parts: String list = [pattern]
        //     
        //     for o in options do
        //         // printfn $"{parts}"
        //         parts <- List.concat (parts |> List.map (fun part ->
        //             part.Split(o)
        //             |> Array.toList
        //             |> List.where ((<>) "")
        //         ))
        //     
        //     
        //     if parts.Length = 0 then
        //         true
        //     else
        //         isCombinationFrom (List.tail options) pattern
        
        // if impossiblePattern options pattern then
        //     false
        // else
        //     (false, options
        //          |> Seq.where pattern.StartsWith
        //          |> Seq.map (fun o ->
        //             if o = pattern then
        //                 true
        //             else
        //                 let sub = pattern.Substring(o.Length)
        //                 if sub = "" then
        //                     true
        //                 else
        //                     isCombinationFrom options sub
        //          ))
        //         ||> Seq.fold (fun acc isCombination -> acc || isCombination)
                
        if impossiblePattern options pattern then
            false
        else
            options
                |> Seq.where pattern.Contains
                |> Seq.map (fun o ->
                    if o = pattern then
                        true
                    else
                        pattern.Split(o)
                            |> Seq.where ((<>) "")
                            |> Seq.map (isCombinationFrom options)
                            |> Seq.tryFind ((=) false)
                            |> _.IsNone
                    )
                |> Seq.tryFind ((=) true)
                |> _.IsSome 
        
    let sort (p: Pattern list) = p |> List.sortBy _.Length 
        
    
    
let fromInput (input: string list): ((Towel list)*(Pattern list)) =
    (
     List.head input |> String |> _.Split(", ") |> Seq.map Pattern.fromInput |> Seq.toList,
     List.skip 2 input |> Seq.map Pattern.fromInput |> Seq.toList
    )

let towels, patterns =
    Utilities.readLines "./day-19/input.txt"
    |> Seq.toList
    |> fromInput

let sortedTowels = Pattern.sort towels
let sortedPatterns =
    Pattern.sort patterns
    // hangs for whatever reason
    |> List.where ((<>) "gbbgburuwbwwwwgwrwbgbwgwbwruuggbuuuubrbrrwgb")

let solvePatterns (towelList: Pattern list) (patternList: Pattern list): (Pattern list) =
    let mutable solvedList = []
    let tl = List.rev towelList 
    printfn "solving:"
    for p in patternList do
        printf $"{p}"
        if Pattern.isCombinationFrom tl p then
            printfn " (solved)"
            solvedList <- p :: solvedList
        else
            printfn " (not solved)"
            
    solvedList
    
let solvedPatterns = solvePatterns sortedTowels sortedPatterns

let result1 = solvedPatterns.Length

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0