(** Try to decrypt a single byte xor-ed ciphertext given the expected
    plaintext lexcion. *)
val single_byte_xor : lexicon: Lexicon.t -> ciphertext: string -> string
