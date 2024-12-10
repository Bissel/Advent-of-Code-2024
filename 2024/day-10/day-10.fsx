#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

type Point = int*int
type Height = int
type Matrix = (Point*Height) list

let data =
    Utilities.readLines("./day-10/input.txt")
    |> Utilities.toMatrix
    |> List.map (fun (x,y,c) -> ((x,y), int (string c)) )

let heightAtPoint (point: Point) (matrix: Matrix): Height =
    matrix |> Seq.find (fun (p,_) -> (=) point p) |> snd
let toHeights (matrix: Matrix): Point list array =
    matrix
    |> Seq.groupBy snd
    |> Seq.sortBy fst
    |> Seq.map (fun a -> snd a |> Seq.map fst |> Seq.toList)
    |> Seq.toArray

let toPaths (points: Point list array): Point list seq =
    let rec _getPath (h: Height) (point: Point): Point list seq =
        if h = 9 then
            seq { yield [point] }
        else
            let X,Y = point
            let higherPoints =
                points[h + 1]
                |> Seq.where (fun (x,y) ->
                    X + 1 = x && Y + 0 = y ||
                    X - 1 = x && Y + 0 = y ||
                    X + 0 = x && Y + 1 = y ||
                    X + 0 = x && Y - 1 = y
                )
                
            let subPaths = higherPoints |> Seq.map (_getPath (h + 1)) |> Seq.toList
            
            if subPaths.Length = 0 then
                seq { yield [point] }
            else
                subPaths
                    |> Seq.map (fun path -> path
                                            |> Seq.map (fun p -> ((X,Y): Point) :: p)
                                            |> Seq.toList
                                        )
                    |> Seq.concat
    
    points[0] |> Seq.map (_getPath 0) |> Seq.concat

let heights = toHeights data
let paths = toPaths heights |> Seq.toList

let fullUniquePaths =
    paths
    |> Seq.where (fun p -> p.Length = 10)
    |> Seq.map (fun points -> (List.head points, List.last points))
    |> Seq.distinct
    |> Seq.toList

// for l in paths do
//     printfn $"{l |> Seq.map Utilities.debugToString |> Utilities.debugConcat }"
//     printfn $"{l |> Seq.map (fun p -> heightAtPoint p data |> string) |> Utilities.debugConcat }"

let result1 = fullUniquePaths.Length

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0