open Utility
open Type

type t =
{
    story : Story.t;
    program_counter : instruction_address;
    frames : Frameset.t;
}

let make story =
{
    story = story;
    program_counter = Story.initial_program_counter story;
    frames = Frameset.make Frame.empty;
}

let current_frame interpreter =
    Frameset.current_frame interpreter.frames

let peek_stack interpreter =
    Frameset.peek_stack interpreter.frames

let pop_stack interpreter =
    { interpreter with frames = Frameset.pop_stack interpreter.frames }

let push_stack interpreter value =
    { interpreter with frames = Frameset.push_stack interpreter.frames value }

let read_local interpreter local =
    Frameset.read_local interpreter.frames local

let write_local interpreter local value =
    { interpreter with frames = Frameset.write_local interpreter.frames local value }

let read_global interpreter global =
    Globals.read interpreter.story global

let write_global interpreter global value =
    { interpreter with story = Globals.write interpreter.story global value }

let read_variable interpreter variable =
    match variable with
    | Stack -> (peek_stack interpreter, pop_stack interpreter)
    | Local_variable local -> (read_local interpreter local, interpreter)
    | Global_variable local -> (read_global interpreter global, interpreter)

let write_variable interpreter value
    match variable with
    | Stack -> push_stack interpreter value
    | Local_variable local -> write_local interpreter local value
    | Global_variable global -> write_global interpreter global value

let read_operand interpreter operand =
    match operand with
    | Large large -> (large, interpreter)
    | Small small -> (small, interpreter)
    | Variable v -> read_variable interpreter v

let operands_to_arguments interpreter operands =
    let rec aux (args, interp) ops =
        match ops with
        | [] -> (args, interp)
        | h :: t ->
            let (argument, new_interpreter) = read_operand interp h in
            aux ((argument :: args), new_interpreter) t in
    let (args_rev, final_interpreter) = aux ([], interpreter) operands in
    ((List.rev args_rev), final_interpreter)

let interpret_store interpreter store result =
    match store with
    | None -> interpreter
    | Some variable -> write_variable interpreter variable result

let display_current_instruction interpreter =
    let address = interpreter.program_counter in
    let instruction = Instruction.decode interpreter.story address in
    Instruction.display instruction interpreter.story

let display interpreter =
    let frames = Frameset.display interpreter.frames in
    let instr = display_current_instruction interpreter in
    Printf.sprintf "\n---\n%s\n%s\n" frames instr
