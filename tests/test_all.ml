open OUnit2

let suite =
  "All" >:::
  [ "Lib" >:::
    [ Test_sstring.suite
    ; Test_lexicon.suite
    ; Test_sstream.suite
    ]
  ; "Challenges" >:::
    [ Set1.suite
    ]
  ]

let () =
  run_test_tt_main suite
