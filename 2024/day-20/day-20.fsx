#load @"../AdventOfCode/Utilities.fs"

open AdventOfCode

type Position = int*int

type Field =
    | Distance of int
    | Wall

type RaceTrack = Field array2d 

module RaceTrack =
    let fromInput (input: char array array) =
        let height = input.Length
        let width = input[0].Length
        let mutable arr = Array2D.create
                            height
                            width
                            Wall
        
        let track = 
            seq {
                for y in [0 .. (height - 1)] do
                    for x in [0 .. (width - 1)] do
                        let c = input[y][x]
                        if c <> '#' then
                            yield ((x,y), c)
            } |> Seq.toList
            
        let trackAt pos = track |> List.find (fun p -> (fst p) = pos)
        
        let mutable field = track |> List.find (fun p -> (snd p) = 'E')
        let mutable lastPos = fst field
        let mutable distance = 0
        
        let isNextPos ((x,y): Position) (last: Position) =
            if x = fst last && y = snd last then
                false
            else
                track |> List.exists (fun ((X,Y),c) -> X = x && Y = y && c <> '#')
        
        while snd field <> 'S' do
            let x,y = fst field
            
            printfn $"{lastPos} | {(x,y)}"
            
            arr[y,x] <- Distance distance
            distance <- distance + 1
            
            if isNextPos         ((x + 1), y) lastPos then
                field <- trackAt ((x + 1), y)
            else if isNextPos    ((x - 1), y) lastPos then
                field <- trackAt ((x - 1), y)
            else if isNextPos    (x, (y + 1)) lastPos then
                field <- trackAt (x, (y + 1))
            else if isNextPos    (x, (y - 1)) lastPos then
                field <- trackAt (x, (y - 1))
                
            if fst field = lastPos then
                failwith "ahhhh"
                
            lastPos <- (x,y)
            
            
        arr
        
    let track (track: RaceTrack): (Position*int) list =
        seq {
            for y in [0 .. (Array2D.length1 track) - 1] do
                for x in [0 .. (Array2D.length2 track) - 1] do
                    if track[y,x] <> Wall then
                         let dist = match track[y,x] with
                                        | Wall -> 0
                                        | Distance i -> i
                         yield ((x,y), dist)
        }
        |> Seq.sortBy snd
        |> Seq.rev
        |> Seq.toList

    let toString (track: RaceTrack) : string =
        seq{ 
            for y in [0 .. (Array2D.length1 track) - 1] do
                for x in [0 .. (Array2D.length2 track) - 1] do
                    yield (
                        match track[y,x] with
                        | Wall -> "#"
                        | Distance i -> $"{i % 10}"
                    )
                yield "\n"
        }
        |> String.concat ""
        
    let findShortCut (trackMap: RaceTrack) (track: (Position*int) list) (pos: Position,distance: int) : int seq =
         let x,y = pos
         let dist p0 p1 sel = (sel p0) - (sel p1)
         let absDist p0 p1 sel = abs ((sel p0) - (sel p1))
         
         track
         |> Seq.where (fun (_, dist) -> dist < (distance - 3))
         |> Seq.where (fun (tPos, _) ->
                let tX, tY = tPos
                if y = tY then
                    if tX = x + 2 then
                        trackMap[y, (x + 1)] = Wall
                    else if tX = x + 3 then
                        trackMap[y, (x + 1)] = Wall &&
                        trackMap[y, (x + 2)] = Wall
                    else if tX = x - 2 then
                        trackMap[y, (x - 1)] = Wall
                    else if tX = x - 3 then
                        trackMap[y, (x - 1)] = Wall &&
                        trackMap[y, (x - 2)] = Wall
                    else
                        false
                else if x = tX then
                    if tY = y + 2 then
                        trackMap[(y + 1), x] = Wall
                    else if tY = y + 3 then
                        trackMap[(y + 1), x] = Wall &&
                        trackMap[(y + 2), x] = Wall
                    else if tY = y - 2 then
                        trackMap[(y - 1), x] = Wall
                    else if tY = y - 3 then
                        trackMap[(y - 1), x] = Wall &&
                        trackMap[(y - 2), x] = Wall
                    else
                        false
                else
                    false
         )
         |> Seq.map (fun (tPos, tDist) -> distance - tDist - (absDist pos tPos fst) - (absDist pos tPos snd))


let trackMap =
    Utilities.readLines "./day-20/input.txt"
    |> Seq.map Seq.toArray  
    |> Seq.toArray
    |> RaceTrack.fromInput

let track = RaceTrack.track trackMap

printfn $"{RaceTrack.toString trackMap}"
printfn $"{track |> Seq.map snd |> Seq.map string |> Utilities.debugConcat }"

let shortCuts =
    track
    |> Seq.map (RaceTrack.findShortCut trackMap track)
    |> Seq.concat
    |> Seq.toList
    
let cutWithLength (cuts: int seq) (len: int)= cuts |> Seq.where ((=) len) |> Seq.length

// for i in [0 .. (track |> List.head |> snd)] do
//     let count = cutWithLength shortCuts i 
//     if count > 0 then
//         printfn $"{i}: {count}"


let result1 = [100 .. (track |> List.head |> snd)]
              |> Seq.map (cutWithLength shortCuts)
              |> Seq.sum

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0