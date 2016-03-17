open Utility
open Type

type t =
{
    dynamic_memory : Immutable_bytes.t;
    static_memory : string;
}

let make dynamic static =
{
    dynamic_memory = Immutable_bytes.make dynamic;
    static_memory = static;
}

let read_byte story address =
    let dynamic_size = Immutable_bytes.size story.dynamic_memory in
    if is_in_range address dynamic_size then
        Immutable_bytes.read_byte story.dynamic_memory address
    else
        let static_addr = dec_byte_addr_by address dynamic_size in
        dereference_string static_addr story.static_memory

let read_word story address =
    let high = read_byte story (address_of_high_byte address) in
    let low = read_byte story (address_of_low_byte address) in
    256 * high + low

let write_byte story address value =
    let dynamic_memory = Immutable_bytes.write_byte story.dynamic_memory address value in
    { story with dynamic_memory }

let write_word story address value =
    let high = (value lsr 8) land 0xFF in
    let low = value land 0xFF in
    let story = write_byte story (address_of_high_byte address) high in
    write_byte story (address_of_low_byte address) low

let header_size = 64

let version_offset = Byte_address 0
let version story =
    match read_byte story version_offset with
    | 1 -> V1
    | 2 -> V2
    | 3 -> V3
    | 4 -> V4
    | 5 -> V5
    | 6 -> V6
    | 7 -> V7
    | 8 -> V8
    | _ -> failwith "unknown version"

let v3_or_lower v =
    match v with
    | V1 | V2 | V3 -> true
    | V4 | V5 | V6 | V7 | V8 -> false

let v4_or_lower v =
    match v with
    | V1 | V2 | V3 | V4 -> true
    | V5 | V6 | V7 | V8 -> false

let v4_or_higher v =
    match v with
    | V1 | V2 | V3 -> false
    | V4 | V5 | V6 | V7 | V8 -> true

let v5_or_higher v =
    match v with
    | V1 | V2 | V3 | V4 -> false
    | V5 | V6 | V7 | V8 -> true

let static_memory_base_offset = Word_address 14

let load filename =
    let file = get_file filename in
    let len = String.length file in
    if len < header_size then
        failwith (Printf.sprintf "%s is not a valid story file" filename)
    else
        let high = dereference_string (address_of_high_byte static_memory_base_offset) file in
        let low = dereference_string (address_of_low_byte static_memory_base_offset) file in
        let dynamic_length = high * 256 + low in
        if dynamic_length > len then
            failwith (Printf.sprintf "%s is not a valid story file" filename)
        else
            let dynamic = String.sub file 0 dynamic_length in
            let static = String.sub file dynamic_length (len - dynamic_length) in
            make dynamic static

let abbreviations_table_base story =
    let abbreviations_table_base_offset = Word_address 24 in
    Abbreviation_table_base (read_word story abbreviations_table_base_offset)

let dictionary_base story =
    let dictionary_base_offset = Word_address 8 in
    Dictionary_base (read_word story dictionary_base_offset)

let object_table_base story =
    let object_table_base_offset = Word_address 10 in
    Object_base (read_word story object_table_base_offset)
