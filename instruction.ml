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

let decode_variable n =
    let maximum_local = 15 in
    if n = 0 then Stack
    else if n <= maximum_local then Local_variable (Local n)
    else Global_variable (Global n)

let encode_variable variable =
    match variable with
    | Stack -> 0
    | Local_variable Local n -> n
    | Global_variable Global n -> n

type operand_type =
    | Large_operand
    | Small_operand
    | Variable_operand
    | Omitted

(* The tables which follow are maps from the opcode identification number
   to the opcode type; the exact order matters. *)

let one_operand_bytecodes = [|
    OP1_128; OP1_129; OP1_130; OP1_131; OP1_132; OP1_133; OP1_134; OP1_135;
    OP1_136; OP1_137; OP1_138; OP1_139; OP1_140; OP1_141; OP1_142; OP1_143  |]

let zero_operand_bytecodes = [|
    OP0_176; OP0_177; OP0_178; OP0_179; OP0_180; OP0_181; OP0_182; OP0_183;
    OP0_184; OP0_185; OP0_186; OP0_187; OP0_188; OP0_189; OP0_190; OP0_191  |]

let two_operand_bytecodes =[|
    ILLEGAL; OP2_1;  OP2_2;  OP2_3;  OP2_4;  OP2_5;   OP2_6;   OP2_7;
    OP2_8;   OP2_9;  OP2_10; OP2_11; OP2_12; OP2_13;  OP2_14;  OP2_15;
    OP2_16;  OP2_17; OP2_18; OP2_19; OP2_20; OP2_21;  OP2_22;  OP2_23;
    OP2_24;  OP2_25; OP2_26; OP2_27; OP2_28; ILLEGAL; ILLEGAL; ILLEGAL |]

let var_operand_bytecodes = [|
    VAR_224; VAR_225; VAR_226; VAR_227; VAR_228; VAR_229; VAR_230; VAR_231;
    VAR_232; VAR_233; VAR_234; VAR_235; VAR_236; VAR_237; VAR_238; VAR_239;
    VAR_240; VAR_241; VAR_242; VAR_243; VAR_244; VAR_245; VAR_246; VAR_247;
    VAR_248; VAR_249; VAR_250; VAR_251; VAR_252; VAR_253; VAR_254; VAR_255 |]

let ext_bytecodes = [|
    EXT_0;   EXT_1;   EXT_2;   EXT_3;   EXT_4;   EXT_5;   EXT_6;   EXT_7;
    EXT_8;   EXT_9;   EXT_10;  EXT_11;  EXT_12;  EXT_13;  EXT_14;  ILLEGAL;
    EXT_16;  EXT_17;  EXT_18;  EXT_19;  EXT_20;  EXT_21;  EXT_22;  EXT_23;
    EXT_24;  EXT_25;  EXT_26;  EXT_27;  EXT_28;  EXT_29;  ILLEGAL; ILLEGAL |]

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
    * In short form, ... the opcode number is given in the bottom 4 bits.
    * In long form, ... the opcode number is given in the bottom 5 bits.
    * In variable form, ... the opcode number is given in the bottom 5 bits.
    * In extended form, ... the opcode number is given in a second opcode byte. *)
    
    (* Now what the spec does not say here clearly is: we have just read 4, 5 or
       8 bits, but we need to figure out which of 100+ opcodes we're talking
       about. The location of the bits depends on the form, but the meaning of
       the bits depends on the operand count. In fact the operand count is far
       more relevant here. It took me some time to puzzle out this section of
       the spec. The spec could more clearly say:
       
    * In extended form the EXT opcode number is given in the following byte. Otherwise:
    * If the operand count is 0OP then the 0OP opcode number is given in the lower 4
      bits.
    * If the operand count is 1OP then the 1OP opcode number is given in the lower 4
      bits.
    * If the operand count is 2OP then the 2OP opcode number is given in the lower 5
      bits.
    * If the operand count is VAR then the VAR opcode number is given in the lower 5
      bits.
    *)
    
    let decode_opcode address form op_count =
        let b = read_byte address in
        match (form, op_count) with
        | (Extended_form, _) ->
            let maximum_extended = 29 in
            let ext = read_byte (inc_byte_addr address) in
            if ext > maximum_extended then ILLEGAL else ext_bytecodes.(ext)
        | (_, OP0) -> zero_operand_bytecodes.(fetch_bits bit3 size4 b)
        | (_, OP1) -> one_operand_bytecodes.(fetch_bits bit3 size4 b)
        | (_, OP2) -> two_operand_bytecodes.(fetch_bits bit4 size5 b)
        | (_, VAR) -> var_operand_bytecodes.(fetch_bits bit4 size5 b) in
    
    let get_opcode_length form =
        match form with
        | Extended_form -> 2
        | _ -> 1 in
    
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

    (* The operand types are large, small or variable, being 2, 1 and 1 bytes
       respectively. We take the list of operand types and produce a list of
       operands. *)
    
    (* This method is not tail recursive but the maximum number of operands
       is eight, so we don't care. *)
    let rec decode_operands operand_address operand_types =
        match operand_types with
        | [] -> []
        | Large_operand :: remaining_types ->
            let w = read_word (byte_addr_to_word_addr operand_address) in
            let tail = decode_operands (inc_byte_addr_by operand_address word_size) remaining_types in
            (Large w) :: tail
        | Small_operand :: remaining_types ->
            let b = read_byte operand_address in
            let tail = decode_operands (inc_byte_addr operand_address) remaining_types in
            (Small b) :: tail
        | Variable_operand :: remaining_types ->
            let b = read_byte operand_address in
            let v = decode_variable b in
            let tail = decode_operands (inc_byte_addr operand_address) remaining_types in
            (Variable v) :: tail
        | Omitted :: _ ->
            failwith "omitted operand type passed to decode operands" in

        let rec get_operand_length operand_types =
            match operand_types with
            | [] -> 0
            | Large_operand :: remaining_types -> word_size + (get_operand_length remaining_types)
            | _ :: remaining_types -> 1 + (get_operand_length remaining_types)
