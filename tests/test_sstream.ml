open OUnit2

let test_fold =
  let test stream expected ctxt =
    let actual = Sstream.fold (+) 0 stream in
    assert_equal ~ctxt ~cmp:[%eq: int] ~printer:[%show: int] expected actual
  in
  "fold" >:::
  [ "Empty" >:: test (Stream.of_list []) 0
  ; "Non empty" >:: test (Stream.of_list [1;2;3;4]) 10
  ]

let suite =
  "Sstream" >:::
  [ test_fold
  ]
