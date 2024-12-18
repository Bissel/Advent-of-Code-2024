#load @"../AdventOfCode/Utilities.fs"
#load @"../AdventOfCode/Vector.fs"

open System
open AdventOfCode

type Point = Vector
module Input =
    // let input = ("example", (6 + 1), 12)
    let input = ("input", (70 + 1), 1024)
    
    let fileName = input |> fun (n,_,_) -> n
    let size = input |> fun (_,s,_) -> s
    let iterationCount = input |> fun (_,_,i) -> i

type MemoryCell = bool
type MemorySpace = MemoryCell array2d 

let inputToPoints (data: string array seq): (int*int) list =
    data |> Seq.map (fun a -> (int a[0], int a[1])) |> Seq.toList 

module MemorySpace =
    let fromInput (points: (int*int) list): MemorySpace =
        Array2D.init
            Input.size
            Input.size
            (fun y x -> List.contains (x,y) points)
            
    let corruptedAt ((x,y): Point) (mem: MemorySpace) : bool =
      x < 0 || y < 0 || x >= Input.size || y >= Input.size || mem[y, x]
      
    let safeAt (point: Point) (mem: MemorySpace) : bool =
      not (corruptedAt point mem)
      
    let print (mem: MemorySpace) =
        for y in [-1 .. Input.size ] do
            for x in [-1 .. Input.size ] do
                printf $"{ if safeAt (x,y) mem then '.' else '#' }"
            printfn ""
    
    let aStar (startPoint: Point) (endPoint: Point) (mem: MemorySpace): int option =
        let successors ((x,y): Point) =
            seq {
                if safeAt (x + 1, y) mem then yield (x + 1, y)
                if safeAt (x - 1, y) mem then yield (x - 1, y)
                if safeAt (x, y + 1) mem then yield (x, y + 1)
                if safeAt (x, y - 1) mem then yield (x, y - 1)
            }
            
        let mutable closedMem = Array2D.init Input.size Input.size (fun _ _ -> false)
        let mutable openList: (Point*int) list = [(startPoint, 0)]
        let mutable distanceToEnd: int option = None
        
        let _expandNode (currentNode: Point) (distance: int) =
            (successors currentNode)
            |> Seq.map (fun p -> (p, distance + 1))
            |> Seq.toList
            
        while not openList.IsEmpty && distanceToEnd.IsNone do
            let currentNode, distance = List.head openList
               
            if currentNode = endPoint then
                distanceToEnd <- Some distance
            else
                closedMem[snd currentNode, fst currentNode] <- true
                openList <- (openList @ (_expandNode currentNode distance)
                             |> List.filter (fun ((x,y),_) -> not closedMem[y,x] ))
                             |> List.sortBy snd
                
        distanceToEnd
        
let memory =
    Utilities.getData $"./day-18/{Input.fileName}.txt" ","
    |> inputToPoints
    |> List.take Input.iterationCount 
    |> MemorySpace.fromInput
    
let startPos = (0,0)
let endPos = (Input.size - 1, Input.size - 1)

// MemorySpace.print memory
let findTarget mem = MemorySpace.aStar startPos endPos mem

let result1 = findTarget memory

printfn $"Result 1: {result1}"



// -------------------------------------------------- //

let data = Utilities.getData $"./day-18/{Input.fileName}.txt" "," |> inputToPoints

let rec findCutoffIteration (iterationLowerBound: int) (iterationUpperBound: int): int option =
    let mem iterationCount = data |> List.take iterationCount |> MemorySpace.fromInput
    
    let midPoint = (iterationUpperBound - iterationLowerBound) / 2 + iterationLowerBound
    
    let tryMidpoint = findTarget (mem midPoint)
    if tryMidpoint.IsNone then
        if iterationLowerBound = iterationUpperBound then
            Some midPoint
        else
            findCutoffIteration iterationLowerBound midPoint
    else
        findCutoffIteration (midPoint + 1) iterationUpperBound

let cutoffIteration = findCutoffIteration Input.iterationCount ((Seq.length data) - 1)
    
let result2 = if cutoffIteration.IsSome then Some data[cutoffIteration.Value - 1] else None
printfn $"Result 2: {result2}"

exit 0