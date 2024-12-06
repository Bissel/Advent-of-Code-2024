#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode
let data = Utilities.readLines "./day-06/input.txt"

let dirU = '^'
let dirR = '>'
let dirD = 'v'
let dirL = '<'
let dirs = [dirU; dirR; dirD; dirL]

let blocked = '#'
let tryBlock = 'O'
let simplePath = 'X'
let pathCross = '+'

module Seq =
    let tap f (s: _ seq) = Seq.map (fun a -> (f a); a ) s

// -------------------------------------------------------------- //

let X (x: int, _: int, _: char) = x
let Y (_: int, y: int, _: char) = y
let C (_: int, _ :int , c: char) = c
let pos (x: int, y: int, _: char) = (x,y)
let point (x: int, y: int) (c: char) = (x,y,c)

let Point (position: int*int) (matrix: (int*int*char) seq)
    = Seq.find (fun point -> (pos point) = position) matrix |> C
    
let IsBlocked (position: int*int) (matrix: (int*int*char) seq) =
    let p = (Point position matrix)
    p = blocked || p = tryBlock

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



let startMatrix = Utilities.toMatrix data

let startPos = startMatrix |> Seq.find (fun point -> (C point) = dirU) |> pos

let nextState (pos: int*int, matrix: (int*int*char) seq) =
    let rotation = Point pos matrix
    let matrixWithPath m = UpdateMatrix (point pos simplePath) m
        
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

// printfn $"{Utilities.debugMatrix mapWithPath}"
// printfn $"ExitPos: {Utilities.debugToString exitPos}"
        
        
let result1 = mapWithPath |> Seq.map C |> Seq.where ((=) simplePath) |> Seq.length




printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let IsOnPath (pos: int*int, rotation: char, matrix: (int*int*char) seq) =
    let field = Point pos matrix
    field = rotation || field = pathCross
    
let nextState2 (pos: int*int, rotation: char, matrix: (int*int*char) seq) =
    let field = Point pos matrix

    let matrixWithPath m =
        let path = if field = '.' || field = rotation
                        then rotation
                        else pathCross
            
        UpdateMatrix (point pos path) m
        
    let nextPos = NextPosition rotation pos
    
    if IsOutsideOfMap (nextPos, matrix) then
        (nextPos, rotation, matrixWithPath matrix)
    else if IsOnPath (nextPos, rotation, matrix) then
        (nextPos, rotation, matrixWithPath matrix)
    else if (IsBlocked nextPos matrix) then
        (pos, RotateRight rotation, matrixWithPath matrix)
    else 
        (nextPos, rotation, matrixWithPath matrix)
        
let rec walk2 (maxSteps: int) (position: int*int, rotation: char, matrix: (int*int*char) list) =
    if IsOutsideOfMap (position, matrix) || Point position matrix = rotation || maxSteps <= 0
        then (position, rotation, matrix)
        else
            let newPosition, newRotation, newMatrix = nextState2 (position, rotation, matrix)
            
            if position = newPosition && rotation = newRotation then
                (newPosition, newRotation, newMatrix)
            else
                walk2 (maxSteps - 1) (newPosition, newRotation, newMatrix)
        
let rec findLoopStarter (maxSteps: int) (blockedPos: (int*int) list) (position: int*int, rotation: char, matrix: (int*int*char) seq) =
    let nextPos = NextPosition rotation position
    
    let next newBlockedPos = findLoopStarter
                                 (maxSteps - 1)
                                 newBlockedPos
                                 (nextState2 (position, rotation, Seq.toList matrix))
    
    if IsOutsideOfMap (nextPos, matrix) then
        blockedPos
    else if IsBlocked nextPos matrix then
        next blockedPos
    else
        let endPos, endRotation, endMatrix = walk2 maxSteps (position, rotation, UpdateMatrix (point nextPos tryBlock) matrix)
        
        // loop auf die Route gefunden
        if not (IsOutsideOfMap (endPos, matrix)) && (IsOnPath (endPos, endRotation, endMatrix)) then
            printfn $"{Utilities.debugMatrix endMatrix}"
            printfn ""
            // printf $" T-{maxSteps} "
            next (nextPos :: blockedPos)
        else
            next blockedPos
        
let findLoopStarter2 (maxSteps: int) (position: int*int, rotation: char, matrix: (int*int*char) list) = 
    let width, height = matrix |> Seq.last |> pos
    
    let mat = UpdateMatrix (point position '.') matrix
    
    let allBlocked = seq {
            for y in [0 .. height] do
                for x in [0 .. width] do
                    let _pos = (x, y)
                    let isPath = Point _pos mapWithPath = simplePath
                    if (not (_pos = position)) && isPath then
                        yield (_pos, UpdateMatrix (point _pos tryBlock) mat)
        }
    
    allBlocked
        |> Seq.map (fun (pos, m) -> (pos, walk2 maxSteps (position, rotation, m)) )
        |> Seq.tap (fun _ -> printf ".")
        |> Seq.where (fun (_, (p, _, m)) -> not (IsOutsideOfMap (p, m)) )
        |> Seq.where (fun (_, (p, r, m)) -> IsOnPath (p, r, m) )
        |> Seq.tap (fun (_, (_, _, m)) ->
            printfn "|"
            printfn $"{Utilities.debugMatrix m}"
        )
            // printfn ""
        |> Seq.map fst
        
let startRotation = Point startPos startMatrix

let blockedPositions = findLoopStarter2
                           // 300 -> 8
                           // 400 -> 11
                           // 1000 -> 81
                           10000 // -> 1946 ✔️
                           (startPos, startRotation, startMatrix)
                           |> Seq.toList
        
// printfn $"{walk2 60 (startPos, startRotation, startMatrix) |> fun (_,_,m) -> m |> Utilities.debugMatrix }"
        
printfn $"Blocked Positions: {blockedPositions |> Seq.map Utilities.debugToString |> Utilities.debugConcat }"
        
let result2 = blockedPositions |> Seq.length
printfn $"Result 2: {result2}"

exit 0