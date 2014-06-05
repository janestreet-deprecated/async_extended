(** Regression test runner. *)
open Qtest_lib.Std;;

let tests = List.concat
  [ Any_error.tests
  ; Coalesced_throttle_test.tests
  ; Delimited_test.tests
  ]

let () = Runner.main tests

