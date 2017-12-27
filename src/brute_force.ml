let single_byte_xor ~lexicon ~ciphertext =
  let open CCOpt.Infix in
  let c_lexicon = Lexicon.of_string ciphertext in
  let decrypted =
    Lexicon.most_frequent_char lexicon >>= fun mfc ->
    Lexicon.most_frequent_char c_lexicon >|= fun c_mfc ->
    Sstring.single_byte_xor ~key:(Cchar.xor mfc c_mfc) ciphertext
  in
  CCOpt.get_or ~default:ciphertext decrypted
