#load @"../AdventOfCode/Utilities.fs"

open System
open AdventOfCode

type Operand = uint64

type Instruction =
    | Adv
    | Bxl
    | Bst
    | Jnz
    | Bxc
    | Out
    | Bdv
    | Cdv

type InstructionCounter = int
type RegisterA = uint64
type RegisterB = uint64
type RegisterC = uint64

type Program = (Instruction*Operand) list

type State = InstructionCounter
            *RegisterA
            *RegisterB
            *RegisterC
            *(string list)
    
module Program =
    let rec fromInput (input: int list) : Program =
        match input with
            | [] -> []
            | 0 :: a :: tail -> (Adv, (uint64 a)) :: fromInput tail
            | 1 :: a :: tail -> (Bxl, (uint64 a)) :: fromInput tail
            | 2 :: a :: tail -> (Bst, (uint64 a)) :: fromInput tail
            | 3 :: a :: tail -> (Jnz, (uint64 a)) :: fromInput tail
            | 4 :: a :: tail -> (Bxc, (uint64 a)) :: fromInput tail
            | 5 :: a :: tail -> (Out, (uint64 a)) :: fromInput tail
            | 6 :: a :: tail -> (Bdv, (uint64 a)) :: fromInput tail
            | 7 :: a :: tail -> (Cdv, (uint64 a)) :: fromInput tail
            | _ :: _ -> failwith "invalid input"
    
module State =
    let ic   ((c,_,_,_,_): State): InstructionCounter = c
    let regA ((_,a,_,_,_): State): RegisterA = a
    let regB ((_,_,b,_,_): State): RegisterB = b
    let regC ((_,_,_,c,_): State): RegisterC = c
    let out  ((_,_,_,_,o): State): string list = o
    
    let comboOp (state: State) (operand: Operand): uint64 =
        match int operand with
            | 0 -> (uint64 0)
            | 1 -> (uint64 1)
            | 2 -> (uint64 2)
            | 3 -> (uint64 3)
            | 4 -> regA state
            | 5 -> regB state
            | 6 -> regC state
            | _ -> failwith "invalid operand"
    
    let literalOp (op: Operand) = op
        
    let fromInput (input: String list) : State*Program =
        (
            (
                0,
                (uint64 input[0]),
                (uint64 input[1]),
                (uint64 input[2]),
                []
            ),
            (input[3] |> _.Split(",") |> Array.map int |> Array.toList |> Program.fromInput)
        )

module Instruction =
    let Call (instruction: Instruction, operand: Operand) (state: State) : State =
        let div (a: uint64): uint64 =
            (State.regA state) >>> (int a)
            
        match instruction with
            // Bitwise XOR
            // RegB <- regB XOR literal op
            | Bxl -> 
                let opA = State.regB state
                let opB = State.literalOp operand
                (
                    (State.ic state) + 1,
                    (State.regA state),
                    (opA ^^^ opB),
                    (State.regC state),
                    (State.out state)
                )
            // RegB <- regB XOR regC
            | Bxc ->
                let opA = State.regB state
                let opB = State.regC state
                (
                    (State.ic state) + 1,
                    (State.regA state),
                    (opA ^^^ opB),
                    (State.regC state),
                    (State.out state)
                )
            // RegB <- comboOp % 8
            | Bst ->
                let opB = State.comboOp state operand
                (
                    (State.ic state) + 1,
                    (State.regA state),
                    (opB % (uint64 8)),
                    (State.regC state),
                    (State.out state)
                )
            // Jump not equal RegA
            | Jnz ->
                let condition = (State.regA state) <> (uint64 0)
                let ic = if condition then int (State.literalOp operand) else ((State.ic state) + 1)
                (
                    ic,
                    (State.regA state),
                    (State.regB state),
                    (State.regC state),
                    (State.out state)
                )
            // Output
            | Out ->
                let op = string ((State.comboOp state operand) % (uint64 8))
                (
                    (State.ic state) + 1,
                    (State.regA state),
                    (State.regB state),
                    (State.regC state),
                    op :: (State.out state)
                )
            // RegA <- Division 
            | Adv ->
                (
                    (State.ic state) + 1,
                    div (State.comboOp state operand),
                    (State.regB state),
                    (State.regC state),
                    (State.out state)
                )
            // RegB <- Division
            | Bdv ->
                (
                    (State.ic state) + 1,
                    (State.regA state),
                    div (State.comboOp state operand),
                    (State.regC state),
                    (State.out state)
                )
            // RegC <- Division
            | Cdv ->
                (
                    (State.ic state) + 1,
                    (State.regA state),
                    (State.regB state),
                    div (State.comboOp state operand),
                    (State.out state)
                )

let data =
    Utilities.getData "./day-17/input.txt" ": "
    |> Seq.map Array.last
    |> Seq.filter ((<>) "")
    |> Seq.toList

let inputState, program = data |> State.fromInput

let run (inputState: State) (program: Program)=
    let programLength = program.Length
    let mutable cState = inputState
    
    while (State.ic cState) < programLength do
        // printfn "----"
        // printfn $"{State.ic cState} {State.regA cState} {State.regB cState} {State.regC cState} -> {program[State.ic cState]}"
        cState <- Instruction.Call program[State.ic cState] cState
        
    cState

let output = State.out (run inputState program)

let result1 = output |> Seq.rev |> String.concat ","

printfn $"Result 1: {result1}"

// -------------------------------------------------- //

let solveForOutput (progOutput: string): uint64 option =
    let expectedOutput = progOutput |> String |> _.Split(",") |> Seq.rev |> Seq.map string |> Seq.toList

    let runB (regA: uint64) (solvedList: string list) = seq {
        let a = regA <<< 3
        for i in [(uint64 0) .. (uint64 7)] do
            let state = (0, (a + i), uint64 0, uint64 0, [])
            let res = State.out (run state program)
            if res = solvedList then
                yield (a + i) 
    }

    let rec runC (programNumbers: string list) (solvedList: string list) (regA: uint64) : uint64 Option =
        match programNumbers with
            | [] -> Some regA
            | head :: tail ->
                let newSolvedList = List.append solvedList [head]
                let res = runB regA newSolvedList
                            |> Seq.map (runC tail newSolvedList)
                            |> Seq.where _.IsSome
                            |> Seq.map _.Value
                            |> Seq.sort
                            
                if Seq.length res > 0 then Some (Seq.head res) else None
                
    runC expectedOutput [] (uint64 0)
                
// uint64 "216584205979245"
let result2 = solveForOutput (data |> List.last)

let resultVerify = Option.map (fun r ->
            State.out ( run (0, r, uint64 0, uint64 0, []) program )
            |> Seq.rev |> String.concat ",")

printfn $"Result 2: {result2} -> {resultVerify result2}"

exit 0