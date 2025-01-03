namespace AdventOfCode

type Vector = int*int
type Vector64 = int64*int64

module Vector =
    let scale (factor: int) (v: Vector) : Vector =
        (fst v * factor, snd v * factor)
        
    let add (a: Vector) (b: Vector) : Vector =
        (fst a + fst b, snd a + snd b)
        
    let sub (a: Vector) (b: Vector): Vector =
        (fst a - fst b, snd a - snd b)
        
    let addi (a: Vector) (b: int) : Vector =
        (fst a + b, snd a + b)
        
    let div (a: Vector) (b: Vector) : Vector =
        if fst b = 0 || snd b = 0 then
            (0,0)
        else
            (fst a / fst b, snd a / snd b)
            
    let allowedChars = "-0123456789,"
    let fromString (s: string) : Vector =
                   s
                   |> Seq.where (fun c -> allowedChars |> Seq.exists ((=)c) )
                   |> Seq.map string
                   |> String.concat ""
                   |> fun (a: System.String) -> a.Split ","
                   |> fun split -> (
                        split[0] |> int,
                        split[1] |> int
                    )

module Vector64 = 
    let scale (factor: int64) (v: Vector64) : Vector64 =
        (fst v * factor, snd v * factor)
        
    let add (a: Vector64) (b: Vector64) : Vector64 =
        (fst a + fst b, snd a + snd b)
        
    let addi (a: Vector64) (b: int64) : Vector64 =
        (fst a + b, snd a + b)
        
    let div (a: Vector64) (b: Vector64) : Vector64 =
        if fst b = 0 || snd b = 0 then
            (0,0)
        else
            (fst a / fst b, snd a / snd b)
            
    let allowedChars = "-0123456789,"
    let fromString (s: string): Vector64 =
           s
           |> Seq.where (fun c -> allowedChars |> Seq.exists ((=)c) )
           |> Seq.map string
           |> String.concat ""
           |> fun (a: System.String) -> a.Split ","
           |> fun split -> (
                split[0] |> int64,
                split[1] |> int64
            )
