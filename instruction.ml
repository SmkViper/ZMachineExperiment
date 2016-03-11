open Utility
open Type

type opcode_form =
    | Long_form
    | Short_form
    | Variable_form
    | Extended_form

type operand_count =
    | OP0
    | OP1
    | OP2
    | VAR

type operand_type =
    | Large_operand
    | Small_operand
    | Variable_operand
    | Omitted

(* Takes the address of an instruction and produces the instruction *)
let decode story (Instruction address) =
    (* Spec 4.3:
    
    Each instruction has a form (long, short, extended or variable) ...
    If the top two bits of the opcode are $$11 the form is variable;
    if $$10, the form is short. If the opcode is 190 ($BE in hexadecimal)
    and the version is 5 or later, the form is "extended". Otherwise,
    the form is "long". *)
    let decode_form address =
        let byte = read_byte address in
        match fetch_bits bit7 size2 byte width
        | 3 -> Variable_form
        | 2 -> if byte = 190 then Extended_form else Short_form
        | _ -> Long_form in
    
    (* Spec:
    * Each instruction has ... an operand count (0OP, 1OP, 2OP or VAR).
    * In short form, bits 4 and 5 of the opcode byte ... If this is $$11
      then the operand count is 0OP; otherwise, 1OP.
    * In long form the operand count is always 2OP.
    * In variable form, if bit 5 is 0 then the count is 2OP; if it is 1,
      then the count is VAR.
    * In extended form, the operand count is VAR. *)
    let decode_op_count address form =
        let b = read_byte address in
        match form with
        | Short_form -> if fetch_bits bit5 size2 b = 3 then OP0 else OP1
        | Long_form -> OP2
        | Variable_form -> if fetch_bits bit5 b then VAR else OP2
        | Extended_form -> VAR in
    
    (* Spec:
    There are four 'types' of operand. These are often specified by a
    number stored in 2 binary digits:
    * $$00 Large constant (0 to 65535) 2 bytes
    * $$01 Small constant (0 to 255) 1 bytes
    * $$10 Variable 1 byte
    * $$11 Omitted altogether 0 bytes *)
    let decode_types n =
        match n with
        | 0 -> Large_operand
        | 1 -> Small_operand
        | 2 -> Variable_operand
        | 3 -> Omitted in
    
    (* Spec 4.4:
    Next, the types of the operands are specified.
    * In short form, bits 4 and 5 of the opcode give the type
    * In long form, bit 6 of the opcode gives the type of the first operand,
      bit 5 of the second. A value of 0 means a small constant and 1 means a
      variable.
    * In variable or extended forms, a byte of 4 operand types is given next.
      This contains 4 2-bit fields: bits 6 and 7 are the first field, bits 0 and
      1 the fourth. The values are operand types as above. Once one type has
      been given as 'omitted', all subsequent ones must be.
    * In the special case of the "double variable" VAR opcodes call_vs2 and
      call_vn2 a second byte of types is given, containing the types for the
      next four operands. *)
    
    (* Once again this could be more clearly written; the spec never calls
       out for instance the obvious fact that 0OP codes have no operand types.
       The logic is:
       
    * If the count is 0OP then there are no operand types.
    * If the count is 1OP then bits 4 and 5 of the opcode give the type
    * In long form the count is 2OP; bit 6 ... *)
    
    (* We walk the byte from low bit pairs -- which correspond to later
       operands -- to high bit pairs, so that the resulting list has
       the first operands at the head and last at the tail *)
    let decode_variable_types type_byte =
        let rec aux i acc =
            if i > 3 then
                acc
            else
                let type_bits = fetch_bits (Bit_number (i * 2 + i)) size2 type_byte in
                match decode_types type_bits with
                | Omitted -> aux (i + 1) acc
                | x -> aux (i + 1) (x :: acc) in
        aux 0 [] in
    
    let decode_operand_types address form op_count opcode =
        match (form, op_count, opcode) with
        | (_, OP0, _) -> []
        | (_, OP1, _) ->
            let b = read_byte address in
            [decode_types (fetch_bits bit5 size2 b)]
        | (Long_form, _, _) ->
            let b = read_byte address in
            (match fetch_bits bit6 size2 b with
            | 0 -> [ Small_operand; Small_operand ]
            | 1 -> [ Small_operand; Variable_operand ]
            | 2 -> [ Variable_operand; Small_operand ]
            | _ -> [ Variable_operand; Variable_operand ])
        | (Variable_form, _, VAR_236)
        | (Variable_form, _, VAR_250) ->
            let opcode_length = get_opcode_length form in
            let type_byte_0 = read_byte (inc_byte_addr_by address opcode_length) in
            let type_byte_1 = read_byte (inc_byte_addr_by address (opcode_length + 1)) in
            (decode_variable_types type_byte_0) @ (decode_variable_types type_byte_1)
        | _ ->
            let opcode_length = get_opcode_length form in
            let type_byte = read_byte (inc_byte_addr_by address opcode_length) in
            decode_variable_types type_byte in
        
        let get_type_length form opcode =
            match (form, opcode) with
            | (Variable_form, VAR_236)
            | (Variable_form, VAR_250) -> 2
            | (Variable_form, _) -> 1
            | _ -> 0 in
