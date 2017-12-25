open OUnit2

let cmp = [%eq: (string, string) Result.result]
let printer = [%show: (string, string) Result.result]

let challenge1 ctxt =
  let open CCResult.Infix in
  let expected = Ok "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" in
  let input =
    "49276d206b696c6c696e6720796f757220627261696e206c\
     696b65206120706f69736f6e6f7573206d757368726f6f6d"
  in
  let actual = Sstring.(of_hex input >|= to_base64) in
  assert_equal ~ctxt ~cmp ~printer expected actual

let challenge2 ctxt =
  let open Sstring in
  let input = "1c0111001f010100061a024b53535009181c" in
  let input' = "686974207468652062756c6c277320657965" in
  let expected = Ok "746865206b696420646f6e277420706c6179" in
  let actual =
    let open CCResult.Infix in
    of_hex input >>= fun input ->
    of_hex input' >>= fun input' ->
    xor input input' >|= fun actual -> to_hex actual
  in
  assert_equal ~ctxt ~cmp ~printer expected actual

let suite =
  "Set 1" >:::
  [ "Base64" >:: challenge1
  ; "Xor" >:: challenge2
  ]
