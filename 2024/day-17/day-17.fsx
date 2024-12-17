#load @"../AdventOfCode/Utilities.fs"

open System
open AdventOfCode

type Operand = int

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
type RegisterA = int
type RegisterB = int
type RegisterC = int

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
            | 0 :: a :: tail -> (Adv, a) :: fromInput tail
            | 1 :: a :: tail -> (Bxl, a) :: fromInput tail
            | 2 :: a :: tail -> (Bst, a) :: fromInput tail
            | 3 :: a :: tail -> (Jnz, a) :: fromInput tail
            | 4 :: a :: tail -> (Bxc, a) :: fromInput tail
            | 5 :: a :: tail -> (Out, a) :: fromInput tail
            | 6 :: a :: tail -> (Bdv, a) :: fromInput tail
            | 7 :: a :: tail -> (Cdv, a) :: fromInput tail
            | _ :: tail -> failwith "invalid input"
    
module State =
    let ic ((ic,_,_,_,_): State): int = ic
    let regA ((_,a,_,_,_): State): int = a
    let regB ((_,_,b,_,_): State): int = b
    let regC ((_,_,_,c,_): State): int = c
    
    let out ((_,_,_,_,o): State): string list = o
    
    let comboOp (state: State) (operand: Operand): int =
        match operand with
            | 0 -> 0
            | 1 -> 1
            | 2 -> 2
            | 3 -> 3
            | 4 -> regA state
            | 5 -> regB state
            | 6 -> regC state
            | _ -> failwith "invalid operand"
    
    let literalOp (op: Operand) = op
        
    let fromInput (input: String list) : State*Program =
        (
            (
                0,
                (int input[0]),
                (int input[1]),
                (int input[2]),
                []
            ),
            (input[3] |> _.Split(",") |> Array.map int |> Array.toList |> Program.fromInput)
        )

module Instruction =
    let Call (instruction: Instruction, operand: Operand) (state: State) : State =
        let div (a:int) : int =
            let opA = State.regA state
            let opB = pown 2 a
            int (opA / opB)

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
                    (opB % 8),
                    (State.regC state),
                    (State.out state)
                )
            // Jump not equal RegA
            | Jnz ->
                let condition = (State.regA state) <> 0
                let ic = if condition then State.literalOp operand else ((State.ic state) + 1)
                (
                    ic,
                    (State.regA state),
                    (State.regB state),
                    (State.regC state),
                    (State.out state)
                )
            // Output
            | Out ->
                let op = string ((State.comboOp state operand) % 8)
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

let inputState, program =
    Utilities.getData "./day-17/input.txt" ": "
    |> Seq.map Array.last
    |> Seq.filter ((<>) "")
    |> Seq.toList
    |> State.fromInput

printfn $"{program}"

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

let result2 = "-"
printfn $"Result 2: {result2}"

exit 0