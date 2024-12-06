#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

let dirU = '^'
let dirR = '>'
let dirD = 'v'
let dirL = '<'
let dirs = [dirU; dirR; dirD; dirL]

let blocked = '#'
let path = 'X'

// -------------------------------------------------------------- //

let X (x: int, _: int, _: char) = x
let Y (_: int, y: int, _: char) = y
let C (_: int, _ :int , c: char) = c
let pos (x: int, y: int, _: char) = (x,y)
let point (x: int, y: int) (c: char) = (x,y,c)

let Point (position: int*int) (matrix: (int*int*char) seq)
    = Seq.find (fun point -> (pos point) = position) matrix |> C
    
let IsBlocked (position: int*int) (matrix: (int*int*char) seq)
    = (Point position matrix) = blocked

let IsOutsideOfMap (position: int*int, matrix: (int*int*char) seq) =
    let width, height = matrix |> Seq.last |> pos
    let x = (fst position)
    let y = (snd position)
    x < 0 || y < 0 || x > width || y > height

let RotateRight (rotation: char) = dirs[ ((Seq.findIndex ((=) rotation) dirs) + 1) % 4 ]
let NextPosition (rotation: char) (pos: int*int) =
    if rotation = dirU then (fst pos, (snd pos) - 1) else
    if rotation = dirR then ((fst pos) + 1, snd pos) else
    if rotation = dirD then (fst pos, (snd pos) + 1) else
    if rotation = dirL then ((fst pos) - 1, snd pos)
    else pos
    
let UpdateMatrix (point: int*int*char) (matrix: (int*int*char) seq) =
    let position = pos point
    matrix
        |> Seq.map (fun p -> if pos p = position then point else p)
        |> Seq.toList

// -------------------------------------------------------------- //


let data = Utilities.readLines "./day-06/input.txt"


let startMatrix = Utilities.toMatrix data

let startPos = startMatrix |> Seq.find (fun point -> (C point) = dirU) |> pos

let nextState (pos: int*int, matrix: (int*int*char) seq) =
    let rotation = Point pos matrix
    let matrixWithPath m = UpdateMatrix (point pos path) m
        
    let nextPos = NextPosition rotation pos
    
    if IsOutsideOfMap (nextPos, matrix) then
        (nextPos, matrixWithPath matrix)
    else if IsBlocked nextPos matrix then
        (pos, UpdateMatrix (point pos (RotateRight rotation)) matrix)
    else 
        (nextPos, matrix |> matrixWithPath |> UpdateMatrix (point nextPos rotation))
        
let rec walk (maxSteps: int) (state: (int*int)*(int*int*char) seq) =
    if IsOutsideOfMap state || maxSteps <= 0
        then state
        else walk (maxSteps - 1) (nextState state) 
        
// let mapWithPath =
//     (startPos, startMatrix)
//     |> nextState
//     |> snd
        
        
let exitPos, mapWithPath = (startPos, startMatrix) |> walk 10000        
        

printfn $"{Utilities.debugMatrix mapWithPath}"
printfn $"ExitPos: {Utilities.debugToString exitPos}"
        
        
let result1 = mapWithPath |> Seq.map C |> Seq.where ((=) path) |> Seq.length




printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0