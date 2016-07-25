open OUnit2
open Sstring

let string_of_chr_codes l =
  let arr = Array.of_list l in
  String.init (Array.length arr) (fun i -> arr.(i) |> Char.chr)

let test_of_hex =
  let cmp = Rresult.equal ~eq:String.equal ~eq_err:String.equal in
  let printer = Rresult.to_string ~print:(fun x -> x) ~print_err:(fun x -> x) in
  let test expected input ctxt =
    assert_equal ~ctxt ~cmp ~printer (Ok (string_of_chr_codes expected)) (of_hex input)
  in
  let test_err err_msg input ctxt =
    assert_equal ~ctxt ~cmp ~printer (Error err_msg) (of_hex input)
  in
  [ "Empty" >:: test [] ""
  ; "Zero" >:: test [0x00] "00"
  ; "255" >:: test [0xff] "ff"
  ; "Padding" >:: test [0x0f] "f"
  ; "Long" >:: test [0x00; 0x01; 0x02; 0x03] "00010203"
  ; "Invalid" >:: test_err "Invalid hex character" "0g"
  ]

let test_to_hex =
  let printer x = x in
  let cmp = String.equal in
  let test expected input ctxt =
    assert_equal ~ctxt ~cmp ~printer expected (input |> string_of_chr_codes |> to_hex)
  in
  [ "Empty" >:: test "" []
  ; "Zero" >:: test "00" [0x00]
  ; "One" >:: test "01" [0x01]
  ; "255" >:: test "ff" [0xff]
  ; "Long" >:: test "0009abcdef" [0x00; 0x09; 0xab; 0xcd; 0xef]
  ]

let test_to_b64 =
  let test expected input ctxt =
    let printer x = x in
    let cmp = String.equal in
    assert_equal ~ctxt ~cmp ~printer expected (to_base64 (string_of_chr_codes input))
  in
  [ "Empty" >:: test "" []
  ; "Zero" >:: test "AAAA" [0x00; 0x00; 0x00]
  ; "One" >:: test "AAAB" [0x00; 0x00; 0x01]
  ; "Max" >:: test "////" [0xff; 0xff; 0xff]
  ; "Padding" >:::
    [ "One" >:: test "AAA=" [0x00; 0x00]
    ; "Two" >:: test "AA==" [0x00]
    ]
  ]

let test_xor =
  let cmp = Rresult.equal ~eq:String.equal ~eq_err:String.equal in
  let printer = Rresult.to_string ~print:(fun x -> x) ~print_err:(fun x -> x) in
  let test expected input input' ctxt =
    let input = string_of_chr_codes input in
    let input' = string_of_chr_codes input' in
    let expected = string_of_chr_codes expected in
    assert_equal ~ctxt ~cmp ~printer (Ok expected) (xor input input')
  in
  let test_err err_msg input input' ctxt =
    let input = string_of_chr_codes input in
    let input' = string_of_chr_codes input' in
    assert_equal ~ctxt ~cmp ~printer (Error err_msg) (xor input input')
  in
  [ "Empty" >:: test [] [] []
  ; "Zero and zero" >:: test [0x00] [0x00] [0x00]
  ; "Zero and one" >:: test [0xff] [0x00] [0xff]
  ; "One and zero" >:: test [0xff] [0xff] [0x00]
  ; "One and one" >:: test [0x00] [0xff] [0xff]
  ; "Long" >:: test [0xff; 0xff] [0x0f; 0x0f] [0xf0; 0xf0]
  ; "Different Length" >:: test_err "Expecting same length strings" [] [0x00]
  ]

let suite =
  [ "Hex" >:::
    [ "Of" >::: test_of_hex
    ; "To" >::: test_to_hex
    ]
  ; "Base64" >:::
    [ "To" >::: test_to_b64
    ]
  ; "Xor" >::: test_xor
  ]
