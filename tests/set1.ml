open OUnit2

let challenge1 ctxt =
  let open Rresult in
  let open Sstring in
  let cmp = Rresult.equal ~eq:String.equal ~eq_err:String.equal in
  let printer = Rresult.to_string ~print:(fun x -> x) ~print_err:(fun x -> x) in
  let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" in
  let input =
    "49276d206b696c6c696e6720796f757220627261696e206c\
     696b65206120706f69736f6e6f7573206d757368726f6f6d"
  in
  assert_equal ~ctxt ~cmp ~printer (Ok expected) (Sstring.(of_hex input >>| to_base64))

let suite =
  [ "Base64" >:: challenge1
  ]
