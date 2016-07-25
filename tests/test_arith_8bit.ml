open OUnit2
open Arith_8bit

let printer = string_of_int

let test_trunc =
  let test expected input ctxt =
    assert_equal ~ctxt ~printer expected (trunc input)
  in
  [ "Zero" >:: test 0 0
  ; "One" >:: test 1 1
  ; "256" >:: test 0 256
  ; "257" >:: test 1 257
  ]

let test_sl =
  let test expected i s ctxt = assert_equal ~ctxt ~printer expected (i << s) in
  [ "NoShift" >:::
    [ "Zero" >:: test 0 0 0
    ; "One" >:: test 1 1 0
    ; "256" >:: test 0 256 0
    ]
  ; "Shift" >:::
    [ "Zero" >:: test 0 0 2
    ; "One" >:: test 4 1 2
    ; "257" >:: test 4 257 2
    ]
  ]

let test_sr =
  let test expected i s ctxt = assert_equal ~ctxt ~printer expected (i >> s) in
  [ "NoShift" >:::
    [ "Zero" >:: test 0 0 0
    ; "One" >:: test 1 1 0
    ; "256" >:: test 0 256 0
    ]
  ; "Shift" >:::
    [ "Zero" >:: test 0 0 2
    ; "One" >:: test 1 4 2
    ; "257" >:: test 1 260 2
    ; "255to63" >:: test 63 0xff 2
    ]
  ]

let suite =
  [ "Truncate" >::: test_trunc
  ; "ShiftLeft" >::: test_sl
  ; "ShiftRight" >::: test_sr
  ]
