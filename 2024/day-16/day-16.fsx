#load @"../AdventOfCode/Utilities.fs"
#load @"../AdventOfCode/Vector.fs"

open AdventOfCode

type Point = Vector
type Start = Point
type End = Point
type Warehouse = bool array2d

type Move =
    | N
    | E
    | S
    | W
    | TurnClockwise
    | TurnCounterClockwise

type State = Start*End*Warehouse

module Move =
    let nextPos (point: Point) (move: Move): Point =
        match move with
            | N -> (fst point, snd point - 1)
            | S -> (fst point, snd point + 1)
            | W -> (fst point - 1, snd point)
            | E -> (fst point + 1, snd point)
            | _ -> point
            
    let rotateMove (rotateWith: Move) (move: Move) : Move =
        match rotateWith with
            | TurnClockwise ->
                match move with
                    | N -> E
                    | E -> S
                    | S -> W
                    | W -> N
                    | _ -> failwith "todo"
            | TurnCounterClockwise ->
                match move with
                    | N -> W
                    | W -> S
                    | S -> E
                    | E -> N
                    | _ -> failwith "todo"
            | _ -> failwith "todo"

module Warehouse =
    
    let at (point: Point) (warehouse: Warehouse) =
      warehouse[snd point, fst point]
    
    let fromInput (input: string seq) : State =
        let matrix = input |> Utilities.toMatrix
        let w,h = matrix |> Seq.map (fun (x,y,_) -> (x,y)) |> Seq.sortDescending |> Seq.head
        let atPos y x = matrix
                          |> Seq.find (fun (_x,_y, _) -> _x = x && _y = y)
                          |> fun (_,_,c) -> c = '#'
        
        (
            (matrix |> Seq.find (fun (_,_, c) -> c = 'S') |> fun (x,y,_) -> (x,y)),
            (matrix |> Seq.find (fun (_,_, c) -> c = 'E') |> fun (x,y,_) -> (x,y)),
            Array2D.init (h + 1) (w + 1) atPos
        )
        
    let aStar ((startPoint, endPoint, warehouse): State): int =
        let successors (point: Point) (move: Move) =
            seq {
                let next = Move.nextPos point move
                if not (at next warehouse) then
                    yield (next, move, 1)
                
                let moveRight = Move.rotateMove Move.TurnClockwise move
                let nextRight = Move.nextPos point moveRight
                if not (at nextRight warehouse) then
                    yield (nextRight, moveRight, 1000 + 1)
                    
                let moveLeft = Move.rotateMove Move.TurnCounterClockwise move
                let nextLeft = Move.nextPos point moveLeft
                if not (at nextLeft warehouse) then
                    yield (nextLeft, moveLeft, 1000 + 1)
            }
            
        
        let _expandNode (currentNode: Point) (move: Move) (distance: int) (closedList: (Point*Move) list) =
            (successors currentNode move)
            |> Seq.where (fun (p,m,_) -> not (Seq.contains (p,m) closedList) )
            |> Seq.map (fun (p,m,d) -> (p,m, distance + d))
            |> Seq.toList
            
        let rec _aStar (fullOpenList: (Point*Move*int) list) (closedList: (Point*Move) list): int =
            match fullOpenList with
                | [] -> -1
                | (currentNode, move, distance) :: openList ->
                    if currentNode = endPoint then
                        distance
                    else
                        let nextClosedList = (currentNode, move) :: closedList
                        let nextOpenList =
                            List.concat [(_expandNode currentNode move distance nextClosedList); openList]
                            |> List.sortBy (fun (_,_,d) -> d)
                        
                        _aStar nextOpenList nextClosedList
                
        _aStar [(startPoint, E, 0)] []
        

let data =
    Utilities.readLines "./day-16/input.txt"
    |> Warehouse.fromInput

// let distance = Warehouse.aStar data

let result1 = 83444 // distance

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

// all paths with length result1
let result2 = "-"
printfn $"Result 2: {result2}"

exit 0