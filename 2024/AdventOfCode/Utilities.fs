namespace AdventOfCode

open System

module Utilities =
    
    let readLines (filePath:string) = seq {
        use sr = new System.IO.StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let getData (filePath: string) (split: string)
      = (readLines filePath)
      |> Seq.map (fun line -> line.Split split)
      
    let toMatrix (rows: string seq) = 
        rows
        |> Seq.map Seq.indexed
        |> Seq.indexed
        |> Seq.map (fun (y, list) -> list |> Seq.map (fun (x, c) -> (x,y,c)) )
        |> Seq.concat
        |> Seq.toList
      
    
    // printfn $"{(sequence |> Seq.map string |> concat)}"
    let debugConcat a = "[" + (String.concat ", " a) + "]"
    let debugToString (a, b) = $"({a},{b})"
    let debugToStringT (a, b, c) = $"({a},{b},{c})"
    
    let debugMatrix (matrix: (int*int*char) seq) =
        matrix
                   |> Seq.groupBy (fun (_,y,_) -> y)
                   |> Seq.map (fun (_,row) -> row |> Seq.map (fun (_,_,c) -> string c) |> (String.concat "") )
                   |> String.concat "\n"

module Seq =
    
    let tap f (s: _ seq) = Seq.map (fun a -> (f a); a ) s
    let tapi f (s: _ seq) = Seq.mapi (fun i a -> (f i a); a ) s
    
    let splitBy (condition: _ -> bool) (input: _ seq) =
        input
          |> Seq.map (fun a -> (condition a, a))
          |> Seq.groupBy fst
          |> Seq.map (fun (_, b) -> Seq.map snd b)
          |> Seq.toArray
          |> fun s -> (s[0], s[1])
          
module EnumUtil =

    /// Return all values for an enumeration type
    let EnumValues<'a> (enumType : Type) : 'a list =
        let values = Enum.GetValues enumType
        let lb = values.GetLowerBound 0
        let ub = values.GetUpperBound 0
        [lb .. ub] |> List.map (fun i -> values.GetValue i :?> 'a) 