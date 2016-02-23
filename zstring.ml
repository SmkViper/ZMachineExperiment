open Utility
open Type

type string_state =
    | Alphabet of int
    | Abbrev of abbreviation_number
    | Leading
    | Trailing of int

let abbrev0 = Abbrev (Abbreviation 0)
let abbrev32 = Abbrev (Abbreviation 32)
let abbrev64 = Abbrev (Abbreviation 64)

let alphabet0 = Alphabet 0
let alphabet1 = Alphabet 1
let alphabet2 = Alphabet 2

let alphabet_table = [|
    [| " "; "?"; "?"; "?"; "?"; "?"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j";
       "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z" |];
    [| " "; "?"; "?"; "?"; "?"; "?"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J";
       "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z" |];
    [| " "; "?"; "?"; "?"; "?"; "?"; "?"; "\n"; "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7";
       "8"; "9"; "."; ","; "!"; "?"; "_"; "#"; "'"; "\""; "/"; "\\"; "-"; ":"; "("; ")" |] |]

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
        let base = first_abbrev_addr (Story.abbreviations_table_base story) in
        let abbr_addr = inc_word_addr_by base n in
        let word_addr = Word_zstring (Story.read_word story abbr_addr) in
        decode_word_address word_addr

let display_bytes story (Zstring addr) =
    let rec aux current acc =
        let word = Story.read_word story current in
        let is_end = fetch_bits bit15 size1 word in
        let zchar1 = fetch_bits bit14 size5 word in
        let zchar2 = fetch_bits bit9 size5 word in
        let zchar3 = fetch_bits bit4 size5 word in
        let s = Printf.sprintf "%02x %s %02x %s %02x %s "
            zchar1 alphabet_table.(zchar1)
            zchar2 alphabet_table.(zchar2)
            zchar3 alphabet_table.(zchar3) in
        let acc = acc ^ s in
        if is_end = 1 then acc
        else aux (inc_word_addr current) acc in
    aux (Word_address addr) ""

(* Note: only processes version 3 strings *)

(* zstrings encode three characters into two-byte words.

The high bit is the end-of-string marker, followed by three
five-bit zchars.

The meaning of the next zchar(s) depends on the current.

If the current zchar is 1, 2 or 3 then the next is an offset
into the abbreviation table; fetch the string indicated there.

If the current zchar is 4 or 5 then the next is an offset into the
uppercase or punctuation alphabets, except if the current is 5
and the next is 6. In that case the two zchars following are a single
10-bit character. *)

let process_zchar (Zchar zchar) state =
    match (zchar, state) with
    | (1, Alphabet _) -> ("", abbrev0)
    | (2, Alphabet _) -> ("", abbrev32)
    | (3, Alphabet _) -> ("", abbrev64)
    | (4, Alphabet _) -> ("", alphabet1)
    | (5, Alphabet _) -> ("", alphabet2)
    | (6, Alphabet 2) -> ("", Leading)
    | (_, Alphabet a) -> (alphabet_table.(a).(zchar), alphabet0)
    | (_, Abbrev Abbreviation a) ->
        let abbrv = Abbreviation (a + zchar) in
        let addr = abbreviation_zstring story abbrev in
        let str = read story addr in
        (str, alphabet0)
    | (_, Leading) -> ("", (Trailing zchar))
    | (_, Trailing high) ->
        let s = string_of_char (Char.chr (high * 32 + zchar)) in
        (s, alphabet0) in
