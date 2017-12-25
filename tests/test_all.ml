open OUnit2

let suite =
  "All" >:::
  [ "Lib" >:::
    [ "Sstring" >::: Test_sstring.suite
    ; Test_lexicon.suite
    ]
  ; "Challenges" >:::
    [ "Set1" >::: Set1.suite
    ]
  ]

let () =
  run_test_tt_main suite
