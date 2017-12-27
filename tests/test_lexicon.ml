open OUnit2

let test_freq =
  let test lexicon expected ctxt =
    let actual = Lexicon.freq lexicon 'a' in
    assert_equal ~ctxt ~cmp:[%eq: float] ~printer:[%show: float] expected actual
  in
  "freq" >:::
  [ "Empty" >:: test Lexicon.empty 0.
  ; "Present" >:: test (Lexicon.of_list ['a', 0.5]) 0.5
  ]

let test_most_frequent_char =
  let test lexicon expected ctxt =
    let actual = Lexicon.most_frequent_char lexicon in
    assert_equal ~ctxt
      ~cmp:[%eq: char option]
      ~printer:[%show: char option]
      expected
      actual
  in
  "most_frequent_char" >:::
  [ "Empty" >:: test Lexicon.empty None
  ; "Not empty" >:: test (Lexicon.of_list ['a', 0.8; 'b', 0.2]) (Some 'a')
  ]

let test_of_string =
  let test input expected ctxt =
    let actual = Lexicon.of_string input in
    assert_equal ~ctxt ~cmp:[%eq: Lexicon.t] ~printer:[%show: Lexicon.t] expected actual
  in
  "of_string" >:::
  [ "Empty" >:: test "" Lexicon.empty
  ; "Single character" >:: test "a" (Lexicon.of_list ['a', 1.])
  ; "Long" >:: test "aaaabbcc" (Lexicon.of_list ['a', 0.5; 'b', 0.25; 'c', 0.25])
  ]

let test_distance =
  let test ~reference ~lexicon ~expected ctxt =
    let actual = Lexicon.distance ~reference lexicon in
    assert_equal ~ctxt ~cmp:[%eq: float] ~printer:[%show: float] expected actual
  in
  let open Lexicon in
  "distance" >:::
  [ "Empty" >:: test ~reference:(of_list ['a', 1.]) ~lexicon:empty ~expected:0.
  ; "Equal" >:: test ~reference:(of_list ['a', 1.]) ~lexicon:(of_list ['a', 1.]) ~expected:0.
  ; "Different" >:: test
      ~reference:(of_list ['a', 0.5; 'b', 0.5; 'c', 0.])
      ~lexicon:(of_list ['a', 0.; 'b', 0.5; 'c', 0.5])
      ~expected:1.
  ]

let test_of_stream =
  let test stream expected ctxt =
    let actual = Lexicon.of_stream stream in
    assert_equal ~ctxt ~cmp:[%eq: Lexicon.t] ~printer:[%show: Lexicon.t] expected actual
  in
  "of_stream" >:::
  [ "Empty" >:: test (Stream.of_list []) Lexicon.empty
  ; "One element" >:: test (Stream.of_list ["ab"]) (Lexicon.of_list ['a', 0.5; 'b', 0.5])
  ; "Two elements" >:: test
      (Stream.of_list ["ab"; "cd"])
      (Lexicon.of_list ['a', 0.25; 'b', 0.25; 'c', 0.25; 'd', 0.25])
  ]

let suite =
  "Lexicon" >:::
  [ test_freq
  ; test_most_frequent_char
  ; test_of_string
  ; test_distance
  ; test_of_stream
  ]
