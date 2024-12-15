#load @"../AdventOfCode/Utilities.fs"
#load @"../AdventOfCode/Vector.fs"

open System
open AdventOfCode

let filename = "input"

type Point = Vector
type Movement =
    | U = '^'
    | D = 'v'
    | L = '>'
    | R = '<'
    
type Obstacle =
    | None = '.'
    | Roboter = '@'
    | Wall = '#'
    | Box = 'O'
    
type Warehouse = Obstacle array2d
type RoboterPosition = Point
type State = Warehouse*RoboterPosition*(Movement list)
    
module Obstacle =
    
    let parse (c: char): Obstacle = 
        match c with
            | '#' -> Obstacle.Wall
            | 'O' -> Obstacle.Box
            | '@' -> Obstacle.Roboter
            | _ -> Obstacle.None
            
    let toString (o: Obstacle) =
        match o with
            | Obstacle.None -> "."
            | Obstacle.Box -> "O"
            | Obstacle.Roboter -> "@"
            | Obstacle.Wall -> "#"
            | _ -> ArgumentOutOfRangeException() |> raise

module Movement =
    
    let tryParse (c: char): Movement Option = 
        match c with
            | '^' -> Some Movement.U
            | 'v' -> Some Movement.D
            | '<' -> Some Movement.L
            | '>' -> Some Movement.R
            | _ -> None
    
    let fromInput (input: string): Movement seq =
        input
        |> Seq.map tryParse
        |> Seq.where _.IsSome
        |> Seq.map _.Value
        
    let toString (m: Movement): string =
        match m with
            | Movement.U -> "^"
            | Movement.D -> "v"
            | Movement.L -> "<"
            | Movement.R -> ">"
            | _ -> ArgumentOutOfRangeException() |> raise

    let getDirection (m: Movement): Vector =
        match m with
            | Movement.U -> ( 0, -1)
            | Movement.D -> ( 0,  1)
            | Movement.L -> (-1,  0)
            | Movement.R -> ( 1,  0)
            | _ -> ArgumentOutOfRangeException() |> raise

module Warehouse =
    let isWall ((x,y): Point) (warehouse: Warehouse) =
        warehouse[y,x] = Obstacle.Wall
        
    let isBox ((x,y): Point) (warehouse: Warehouse) =
        warehouse[y,x] = Obstacle.Box
        
    let noObstacle ((x,y): Point) (warehouse: Warehouse) =
        warehouse[y,x] = Obstacle.None
        
    let fromInput (input: string seq) : Warehouse =
        let matrix = input |> Utilities.toMatrix
        let w,h = matrix |> Seq.map (fun (x,y,_) -> (x,y)) |> Seq.sortDescending |> Seq.head
        let atPos y x = matrix
                          |> Seq.find (fun (_x,_y, _) -> _x = x && _y = y)
                          |> fun (_,_,c) -> Obstacle.parse c
        
        Array2D.init (h + 1) (w + 1) atPos
        
    let removeRobot ((x,y): Point) (w: Warehouse): Warehouse =
        let mutable warehouse = w
        warehouse[y,x] <- Obstacle.None
        warehouse
        
    let find (o: Obstacle) (warehouse: Warehouse): Point =
        seq {
            for y in [0 .. Array2D.length1 warehouse - 1] do
                for x  in [0 .. Array2D.length2 warehouse - 1] do
                    if warehouse[y, x] = o then
                        yield (x,y)
        } |> Seq.head
        
    let toString (roboter: Point) (warehouse: Warehouse): string =
        seq {
            for y in [0 .. Array2D.length1 warehouse - 1] do
                for x  in [0 .. Array2D.length2 warehouse - 1] do
                    yield Obstacle.toString
                              (if (x,y) = roboter then Obstacle.Roboter else warehouse[y, x])
                yield "\n"
        }
        |> String.concat ""    
        
    let next (warehouse: Warehouse) (pos: Point) (move: Movement): (Warehouse*Point)  =
        let direction = Movement.getDirection move
        let nextPos = Vector.add pos direction
        
        if isWall nextPos warehouse then
            (warehouse, pos)
        else if isBox nextPos warehouse then
            // find first empty spot
            let mutable boxPos = nextPos
            while isBox boxPos warehouse do
                boxPos <- Vector.add boxPos direction
            
            // box can not be moved through wall
            if isWall boxPos warehouse then
                (warehouse, pos)
            else
                // move box
                let mutable nextWarehouse = warehouse
                nextWarehouse[snd nextPos, fst nextPos] <- Obstacle.None
                nextWarehouse[snd boxPos, fst boxPos] <- Obstacle.Box
                (nextWarehouse, nextPos)
        else
            let nextWarehouse = warehouse
            (nextWarehouse, nextPos)
            
    let boxesGps (warehouse: Warehouse): int seq = seq {
        for y in [0 .. Array2D.length1 warehouse - 1] do
            for x  in [0 .. Array2D.length2 warehouse - 1] do
                if isBox (x,y) warehouse then
                    yield 100 * y + x
    }
                

module State =
    let warehouse ((w, _, _): State): Warehouse = w
    let position ((_, p, _): State): Point = p
    let moves ((_, _, m): State): Movement list = m
    
    let rec move (state: State): State =
        let mutable warehouse = warehouse state
        let mutable position = position state
        
        printfn ""
        for move in moves state do
            printf $"\r{Movement.toString move}"
            let a = Warehouse.next warehouse position move
            warehouse <- fst a
            position <- snd a
            
        printfn ""
            
        (warehouse, position, [])
            
let convertInput (input: string seq) : State =
    let emptyRowIndex = Seq.findIndex ((=)"") input
    let warehouse = input |> Seq.take emptyRowIndex |> Warehouse.fromInput
    let movement = input |> Seq.skip (emptyRowIndex + 1) |> String.concat "" |> Movement.fromInput
    let position = warehouse |> Warehouse.find Obstacle.Roboter
    (
        warehouse |> Warehouse.removeRobot position,
        position,
        movement |> Seq.toList
    )

let data = Utilities.readLines $"./day-15/{filename}.txt" |> convertInput

// printfn $"{State.warehouse data |> Warehouse.toString (State.position data) }"
// printfn $"{State.position data }"
// printfn $"""{State.moves data |> Seq.map Movement.toString |> (String.concat "") }"""

let newState = State.move data
printfn $"{State.warehouse newState |> Warehouse.toString (State.position newState) }"

let result1 = Warehouse.boxesGps (State.warehouse newState) |> Seq.sum

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0