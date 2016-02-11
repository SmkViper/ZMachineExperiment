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