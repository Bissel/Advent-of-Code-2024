#load @"../AdventOfCode/Utilities.fs"
#load @"../AdventOfCode/Vector.fs"

open AdventOfCode

module Input =
    // input-file * (width * height)
    let input = ("input", (11, 7))
    // let input = ("input", (101, 103))
    
    let file = fst input
    let width = fst (snd input)
    let height = snd (snd input)

module Seq =
    let product (values: int seq): int =
        (1, values) ||> Seq.fold (*)

// 0,0 -> Top-Left
// x = Top -> Bottom
// y = Left -> Right
type Position = Vector
type Velocity = Vector
type Robot = Position*Velocity

module Robot =
    let lineToRobot (line: string array): Robot =
        (Vector.fromString line[0], Vector.fromString line[1])
        
    let position ((p,_): Robot) = p
    let velocity ((_,v): Robot) = v
    
    let nextState (areaSize: int*int) (robot: Robot): Robot =
        let _wrapPos (sel: Vector -> int) (pos: Vector) =
            let aPos = sel areaSize
            let cPos = sel pos
            if cPos < 0 then
                aPos + cPos
            else if cPos >= aPos then
                cPos - aPos
            else
                cPos
        
        let pos = position robot
        let vel = velocity robot
        
        let calcPos = Vector.add pos vel
        let nextPos = (_wrapPos fst calcPos, _wrapPos snd calcPos)
        
        (nextPos, vel)

type Area =
    (int*int)*(Robot list)
module Area =
    let w (area: Area): int = fst (fst area)
    let h (area: Area): int = snd (fst area)
    
    let robots (area: Area) = snd area
    
    let nextState ((size,robots): Area): Area = (
        size,    
        robots |> List.map (Robot.nextState size) 
    )
    
    let quadrants ((size, robots): Area): Area list =
        let qSize = Vector.div size (2,2)
        
        let top0 = 0
        let bottom0 = snd qSize
        let top1 = snd size - snd qSize
        let bottom1 = snd size
            
        let left0 = 0
        let right0 = fst qSize
        let left1 = fst size - fst qSize
        let right1 = fst size
        [
            ((left0, top0), (right0, bottom0));
            ((left1, top0), (right1, bottom0));
            ((left0, top1), (right0, bottom1));
            ((left1, top1), (right1, bottom1))
        ]
        |> List.map (fun (q0, q1) -> (
                (Vector.sub q1 q0),
                robots
                 |> List.where (fun r ->
                    let x,y = Robot.position r
                    x >= fst q0 && y >= snd q0 &&
                    x < fst q1 && y < snd q1
                 )
                 |> List.map (fun r ->
                    let x,y = Robot.position r
                    let vel = Robot.velocity r
                    
                    let _x = if x >= fst q0 then x - fst q0 else x
                    let _y = if y >= snd q0 then y - snd q0 else y
                    ( (_x, _y), vel )
                )
            )
        )
    
    let toString (area: Area) =
        seq {
            for y in [0 .. ((h area) - 1)] do
                for x in [0 .. ((w area) - 1)] do
                    yield robots area
                          |> Seq.where (fun r -> (Robot.position r) = (x,y))
                          |> Seq.length
                          |> fun l -> if l = 0 then "." else string l 
                yield "\n"
        } |> String.concat "" 
        
let data: Area =
    (
        (Input.width, Input.height),
        (Utilities.getData $"./day-14/{Input.file}.txt" " "
        |> Seq.map Robot.lineToRobot
        |> Seq.toList)
    )
    
let doSteps (n: int) (area:Area): Area =
    let mutable a = area
    // printfn "Start:"
    // printfn $"{a |> Area.toString}"
    for i in [1 .. n] do
        a <- Area.nextState a
        // if i = 100 then
        //     printfn ""
        //     printfn $"After {i} seconds:"
        //     printfn $"{a |> Area.toString}"
    a
    
let after100 = doSteps 100 data
let quarters = Area.quadrants after100
let result1 = quarters
              |> Seq.map (fun a -> Area.robots a |> _.Length)
              |> Seq.product

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let findTree (maxSteps: uint) (area: Area): (int64*Area option) =
    let midPointX = Area.w area / 2 - 1
    let midPointY = Area.h area / 2 - 1
    
    let rec _findTree (steps: uint) (area: Area) = 
        if steps = (uint 0) then
            ((int64 -1), None)
        else
            if (int steps) % 100_000 = 0 then
                printfn $"{maxSteps - steps}"
            let robots = Area.robots area
            let hasPoint point = robots |> List.exists (fun (p,_) -> p = point)
            if
                // hasPoint (midPoint, 0) &&
                hasPoint (midPointX, midPointY - 2) &&
                hasPoint (midPointX, midPointY - 3) &&
                hasPoint (midPointX, midPointY - 4) &&
                hasPoint (midPointX, midPointY - 5) &&
                hasPoint (midPointX, midPointY - 6) 
                // hasPoint (midPoint - 1, 1) &&
                // hasPoint (midPoint + 1, 1) //&&
                // hasPoint (midPoint, 2) &&
                // hasPoint (midPoint - 2, 2) &&
                // hasPoint (midPoint + 2, 2)
            then
                printfn $"{Area.toString area}"
                ((int64 maxSteps) - (int64 steps), Some area)
            else
                _findTree (steps - (uint 1)) (Area.nextState area)
        
    _findTree maxSteps area


let p,_ = findTree (uint 100000000) data

let result2 = p
printfn $"Result 2: {result2}"

exit 0