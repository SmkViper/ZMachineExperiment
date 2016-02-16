open Utility
open Type

let abbreviation_table_length = 96

(* A "word address" is only used in the abbreviation table, and is always
just half the real address. A "packed address" is used in calls and fetching
strings, and is half the real address in v3 but different for other versions. *)

let decode_word_address (Word_zstring word_address) =
    Zstring (word_address * 2)

let first_abbrev_addr (Abbreviation_table_base base) =
    Word_address base

let abbreviation_zstring story (Abbreviation n) =
    if n < 0 || n >= abbreviation_table_length then
        failwith "bad offset into abbreviation table"
    else
        let base = first_abbrev_addr (Story.abbreviations_table_base story)
        let abbr_addr = inc_word_addr_by base n in
        let word_addr = Word_zstring (Story.read_word story abbr_addr) in
        decode_word_address word_addr
