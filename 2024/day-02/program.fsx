open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let data = (readLines "./input.txt")
           |> Seq.map (fun line -> line.Split " ")
           |> Seq.map (Array.map int);

// The engineers are trying to figure out which reports are safe.
// The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing
// or gradually decreasing. So, a report only counts as safe if both of the following are true:
//
// - The levels are either all increasing or all decreasing.
// - Any two adjacent levels differ by at least one and at most three.

let between min max value = value >= min && value <= max

let pairIncreasing (a, b) = a < b
let pairDecreasing (a, b) = a > b
let pairDifferSave (a, b) = between 1 3 (abs (a - b))

let checkPairwise (compare: int * int -> bool, levels: int array)
  = levels
  |> Array.pairwise
  |> Array.map compare
  |> Array.reduce (fun a b -> a && b)

let isSave (levels: int array)
  = (checkPairwise(pairIncreasing, levels) || checkPairwise(pairDecreasing, levels))
  && checkPairwise(pairDifferSave, levels)

let tolerateLevels = data |> Seq.map isSave

// Analyze the unusual data from the engineers. How many reports are safe?
let result1 = tolerateLevels |> Seq.map (fun a -> if a then 1 else 0) |> Seq.sum
printfn $"Result 1: {result1}"

// Now, the same rules apply as before, except if removing a single level from an unsafe
// report would make it safe, the report instead counts as safe.

let permutation (levels: int array) = seq {
    for index in 0 .. levels.Length - 1 do
        let head = Array.take index levels
        let tail = Array.skip (index + 1) levels
        yield Array.append head tail
}

let checkAllPermutations (levels: int array) 
  = isSave levels
    || permutation levels |> Seq.map isSave |> Seq.reduce (fun a b -> a || b)

let moreTolerateLevels = data |> Seq.map checkAllPermutations

let result2 = moreTolerateLevels |> Seq.map (fun a -> if a then 1 else 0) |> Seq.sum
printfn $"Result 2: {result2}"
