#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

let data = Utilities.getData("./day-01/input.txt", "   ")
           |> Seq.map (fun s -> (s[0], s[1]))
           |> Seq.map (fun (a,b) -> (int a, int b))


// Pair up the smallest number in the left list with the smallest number in the right list,
// then the second-smallest left number with the second-smallest right number, and so on.
let list0 = data |> Seq.map fst |> Seq.sort
let list1 = data |> Seq.map snd |> Seq.sort

let pairs = Seq.zip list0 list1

// Within each pair, figure out how far apart the two numbers are; you'll need to add up all of those distances

let distances = pairs |> Seq.map (fun (a,b) -> abs (a - b) )

// Sum
let result1 = Seq.sum distances;
printfn $"Result 1: {result1}"

// This time, you'll need to figure out exactly how often each number from the
// left list appears in the right list. Calculate a total similarity score by adding
// up each number in the left list after multiplying it by the number of times that number
// appears in the right list.
let similarityScore = list0
                    |> Seq.map (fun number -> (
                        number,
                        list1 |> Seq.where (fun i -> i = number) |> Seq.length
                    ))
                    |> Seq.map (fun (a, b) -> a * b)

let result2 = Seq.sum similarityScore;
printfn $"Result 2: {result2}"

exit 0