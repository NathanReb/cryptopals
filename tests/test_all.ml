open OUnit2

let suite =
  "All" >:::
  [ "Lib" >:::
    [ "Arith_8bit" >::: Test_arith_8bit.suite
    ; "Sstring" >::: Test_sstring.suite
    ]
  ; "Challenges" >:::
    [ "Set1" >::: Set1.suite
    ]
  ]

let () =
  run_test_tt_main suite
