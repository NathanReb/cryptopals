module Single_byte_xor = struct
  let mfc ~lexicon ~ciphertext =
    let open CCOpt.Infix in
    let c_lexicon = Lexicon.of_string ciphertext in
    let decrypted =
      Lexicon.most_frequent_char lexicon >>= fun mfc ->
      Lexicon.most_frequent_char c_lexicon >|= fun c_mfc ->
      Sstring.single_byte_xor ~key:(Cchar.xor mfc c_mfc) ciphertext
    in
    CCOpt.get_or ~default:ciphertext decrypted

  let plaintext_distance ~lexicon ~ciphertext =
    if String.equal ciphertext "" then
      ciphertext
    else
      let keys = CCList.init 256 char_of_int in
      let best_plaintext, _ =
        CCList.fold_left
          ( fun (best_plaintext, best_distance) key ->
              let plaintext = Sstring.single_byte_xor ~key ciphertext in
              let lexicon' = Lexicon.of_string plaintext in
              let distance = Lexicon.distance ~reference:lexicon lexicon' in
              if distance < best_distance then
                (plaintext, distance)
              else
                (best_plaintext, best_distance)
          )
          ("", max_float)
          keys
      in
      best_plaintext
end

let single_byte_xor = Single_byte_xor.plaintext_distance
