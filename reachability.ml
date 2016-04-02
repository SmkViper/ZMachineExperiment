open Utility
open Type

(* Any given instruction in a routine either goes on to the next instruction
when it is done, or branches to another instruction when it is done, or
terminates the routine. Given the address of an instruction, what are all the
reachable instructions in this routine? Note that this could miss instructions
if a jump is made to a location read from a variable. *)

let following_instruction instr =
    if Instruction.continues_to_following (Instruction.opcode instr) then
        let (Instruction addr) = (Instruction.address instr) in
        let length = (Instruction.length instr) in
        [Instruction (addr + length)]
    else
        []

let branch_target_instruction instr =
    match Instruction.branch instr with
    | None
    | Some (_, Return_false)
    | Some (_, Return_true) -> []
    | Some (_, Branch_address address) -> [address]

let jump_target_instruction instr =
    match (Instruction.opcode instr, Instruction.operands instr) with
    | (OP1_140, [Large offset]) ->
        let offset = signed_word offset in
        [Instruction.jump_address instr offset]
    | _ -> []
