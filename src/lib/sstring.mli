val of_hex : string -> (string, string) result
val to_hex : string -> string

val to_base64 : string -> string

val xor : string -> string -> (string, string) result

val single_byte_xor : key: char -> string -> string

val fold : ('a -> char -> 'a) -> 'a -> string -> 'a
