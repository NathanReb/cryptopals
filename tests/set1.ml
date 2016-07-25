open OUnit2

let cmp = Rresult.equal ~eq:String.equal ~eq_err:String.equal
let printer = Rresult.to_string ~print:(fun x -> x) ~print_err:(fun x -> x)

let challenge1 ctxt =
  let open Rresult in
  let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" in
  let input =
    "49276d206b696c6c696e6720796f757220627261696e206c\
     696b65206120706f69736f6e6f7573206d757368726f6f6d"
  in
  assert_equal ~ctxt ~cmp ~printer (Ok expected) (Sstring.(of_hex input >>| to_base64))

let challenge2 ctxt =
  let open Rresult in
  let open Sstring in
  let input = "1c0111001f010100061a024b53535009181c" in
  let input' = "686974207468652062756c6c277320657965" in
  let expected = "746865206b696420646f6e277420706c6179" in
  let real =
    of_hex input >>> fun input ->
    of_hex input' >>> fun input' ->
    xor input input' >>| fun real -> to_hex real
  in
  assert_equal ~ctxt ~cmp ~printer (Ok expected) real

let suite =
  [ "Base64" >:: challenge1
  ; "Xor" >:: challenge2
  ]
