open OUnit2

let assert_equal_string_result ~ctxt =
  assert_equal ~ctxt
    ~cmp:[%eq: (string, string) Result.result]
    ~printer:[%show: (string, string) Result.result]

let test_of_hex =
  let test input expected ctxt =
    let actual = Sstring.of_hex input in
    assert_equal_string_result ~ctxt expected actual
  in
  "of_hex" >:::
  [ "Empty" >:: test "" (Ok "")
  ; "Zero" >:: test "00" (Ok "\x00")
  ; "255" >:: test "ff" (Ok "\xff")
  ; "Padding" >:: test "f" (Ok "\x0f")
  ; "Long" >:: test "00010203" (Ok "\x00\x01\x02\x03")
  ; "Invalid" >:: test "0g" (Error "Invalid hex character")
  ]

let test_to_hex =
  let test input expected ctxt =
    let actual = Sstring.to_hex input in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "to_hex" >:::
  [ "Empty" >:: test "" ""
  ; "Zero" >:: test "\x00" "00"
  ; "One" >:: test "\x01" "01"
  ; "255" >:: test "\xff" "ff"
  ; "Long" >:: test "\x00\x09\xab\xcd\xef" "0009abcdef"
  ]

let test_to_b64 =
  let test input expected ctxt =
    let actual = Sstring.to_base64 input in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "to_b64" >:::
  [ "Empty" >:: test "" ""
  ; "Zero" >:: test "\x00\x00\x00" "AAAA"
  ; "One" >:: test "\x00\x00\x01" "AAAB"
  ; "Max" >:: test "\xff\xff\xff" "////"
  ; "Padding" >:::
    [ "One" >:: test "\x00\x00" "AAA="
    ; "Two" >:: test "\x00" "AA=="
    ]
  ]

let test_xor =
  let test input input' expected ctxt =
    let actual = Sstring.xor input input' in
    assert_equal_string_result ~ctxt expected actual
  in
  "xor" >:::
  [ "Empty" >:: test "" "" (Ok "")
  ; "Zero and zero" >:: test "\x00" "\x00" (Ok "\x00")
  ; "Zero and one" >:: test "\xff" "\x00" (Ok "\xff")
  ; "One and zero" >:: test "\x00" "\xff" (Ok "\xff")
  ; "One and one" >:: test "\xff" "\xff" (Ok "\x00")
  ; "Long" >:: test "\x0f\x0f" "\xf0\xf0" (Ok "\xff\xff")
  ; "Different Length" >:: test "" "\x00" (Error "Expecting same length strings")
  ]

let suite =
  "Sstring" >:::
  [ test_of_hex
  ; test_to_hex
  ; test_to_b64
  ; test_xor
  ]
