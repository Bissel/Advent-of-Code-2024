﻿namespace AdventOfCode

module Utilities =
    
    let readLines (filePath:string) = seq {
        use sr = new System.IO.StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let getData (filePath: string, split: string)
      = (readLines filePath)
      |> Seq.map (fun line -> line.Split split)
      
    
    // printfn $"{(sequence |> Seq.map string |> concat)}"
    let debugConcat a = "[" + (String.concat ", " a) + "]"
    let debugToString (a, b) = $"({a},{b})"