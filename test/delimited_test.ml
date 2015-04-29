(** Tests for Delimited. *)

open Core.Std
open Async.Std
open Async_extended.Std
open Qtest_lib.Std

let test_csv_with_header () =
  Delimited.Csv.create_reader ~header:`Yes "orders.csv"
  >>= fun pipe_reader ->
  Pipe.to_list pipe_reader
  >>| function
    | [ row1; row2; _row3 ] ->
      assert_string_equal (Delimited.Row.get_exn row1 "symbol") "EPP US";
      assert_string_equal (Delimited.Row.get_exn row2 "account") "10147580"
    | rows -> failwithf "Expected 3 rows, got %d" (List.length rows) () 

let test_csv_sans_header () =
  Delimited.Csv.create_reader ~header:`No "orders.csv"
  >>= fun pipe_reader ->
  Pipe.to_list pipe_reader
  >>| fun rows ->
  assert_equal ~sexp_of_t:Int.sexp_of_t 4 (List.length rows)

let tests = [
  "Delimited_test.test_csv_with_header", test_csv_with_header;
  "Delimited_test.test_csv_sans_header", test_csv_sans_header;
  ]
