#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

let data =
    Utilities.readLines "./day-12/input.txt"
    |> Utilities.toMatrix
   
let width, height, _ = data |> List.last

printfn $"{width} {height}"
let matrix = Array2D.init
                 (width + 1)
                 (height + 1)
                 (fun X Y -> data |> List.find (fun (x,y,_) -> x = X && y = Y ) |> fun (_,_,c) -> c )

// has boundaries: Top, Right, Bottom, Left
type Boundarie = (int*int)*(int*int)

let getBoundaries (data: char array2d): Boundarie seq =
    let bounds =
        Array2D.init
            (width + 1)
            (height + 1)
            (fun x y ->
                let c = data[x,y]
                (
                    y = 0      || data[x, y - 1] <> c,
                    x = width  || data[x + 1, y] <> c,
                    y = height || data[x, y + 1] <> c,
                    x = 0      || data[x - 1, y] <> c
                )
            )
            
    seq {
        for x in [0 .. width] do
            for y in [0 .. height] do
                let t,r,b,l = bounds[x,y]
                if t then yield ((x, y), (x, y - 1))
                if r then yield ((x, y), (x + 1, y))
                if b then yield ((x, y), (x, y + 1))
                if l then yield ((x, y), (x - 1, y))
    }

type Region = char * (int*int) list
type Perimeter = int

let getRegions (data: char array2d): Region list =
    let mutable areas: (char * (int*int) list) list = []
    
    let findIndex (c: char) (pos: int*int) =
        areas
        |> List.tryFindIndex (fun x -> fst x = c && snd x |> Seq.exists ((=) pos))
    
    let addPosToArea index pos = 
        areas |> List.mapi (fun i area ->
            if i <> index then area
            else (
                fst area,
                List.append [pos] (snd area)
            )
        )
        
    let mergeAreas c aIndex bIndex =
        let positionsA = snd areas[aIndex]
        let positionsB = snd areas[bIndex]
        areas
            |> List.indexed
            |> List.where (fun (i, _) -> i <> aIndex && i <> bIndex)
            |> List.map snd
            |> List.append [(c, List.concat [positionsA; positionsB])]
        
    let createNewArea c pos =
        List.append [(c, [pos])] areas
        
    for y in [0 .. height] do
        for x in [0 .. width] do
            let pos = (x,y)
            let c = data[x,y]
            
            let leftAreaIndex = findIndex c (x - 1, y)
            let topAreaIndex = findIndex c (x, y - 1)
            
            let isLeft = leftAreaIndex.IsSome
            let isTop = topAreaIndex.IsSome
            
            if isTop && isLeft && topAreaIndex <> leftAreaIndex then
                areas <- addPosToArea leftAreaIndex.Value pos
                areas <- mergeAreas c leftAreaIndex.Value topAreaIndex.Value                    
            else if isLeft then
                areas <- addPosToArea leftAreaIndex.Value pos
            else if isTop then
                areas <- addPosToArea topAreaIndex.Value pos
            else
                areas <- createNewArea c pos
    
    areas 

let getPerimeter (bounds: Boundarie seq) (region: Region): int =
    snd region
    |> Seq.map (fun pos -> bounds |> Seq.where (fun (a,_) -> a = pos) |> Seq.length)
    |> Seq.sum
    
let bounds = getBoundaries matrix |> Seq.toList
let regions = getRegions matrix
let regionPerimeters = regions |> Seq.map (getPerimeter bounds) |> Seq.toList
let regionWithPerimeters = Seq.zip (regions |> Seq.map snd |> Seq.map Seq.length ) regionPerimeters
    
let result1 = regionWithPerimeters |> Seq.map (fun (a,b) -> a * b ) |> Seq.sum

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0