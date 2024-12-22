#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode


// Calculate the result of
//   multiplying the secret number by 64.
//   Then, mix this result into the secret number.
//   Finally, prune the secret number.
// Calculate the result of
//   dividing the secret number by 32.
//   Round the result down to the nearest integer.
//   Then, mix this result into the secret number.
//   Finally, prune the secret number.
// Calculate the result of
//   multiplying the secret number by 2048.
//   Then, mix this result into the secret number.
//   Finally, prune the secret number.
// To mix a value into the secret number,
//   calculate the bitwise XOR of the given value and the secret number.
//   Then, the secret number becomes the result of that operation.
//   (If the secret number is 42 and you were to mix 15 into the secret number,
//   the secret number would become 37.)
// To prune the secret number,
//   calculate the value of the secret number modulo 16777216.
//   Then, the secret number becomes the result of that operation.
//   (If the secret number is 100000000 and you were to prune the secret number,
//   the secret number would become 16113920.)
let calcSecret (num: int64): int64 =
    let mul64 n = n <<< 6
    let div32 n = n >>> 5
    let mul2048 n = n <<< 11
    let mix secret n  = secret ^^^ n
    let prune n = n % (int64 16777216)
    
    let mutable secret = num
    
    secret <- secret |> mul64   |> (mix secret) |> prune
    secret <- secret |> div32   |> (mix secret) |> prune
    secret <- secret |> mul2048 |> (mix secret) |> prune
    
    secret

let doSteps (n: int) (secret: int64): int64 =
    let mutable s = secret
    for _ in [1 .. n] do
        s <- (calcSecret s)
    s

let data =
    Utilities.readLines "./day-22/input.txt"
    |> Seq.map int64
    |> Seq.toList


let secretsAfter2000 = data |> Seq.map (doSteps 2000) |> Seq.sum 

let result1 = secretsAfter2000

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0