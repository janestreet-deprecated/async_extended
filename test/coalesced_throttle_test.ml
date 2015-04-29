(** Tests for Coalesced_throttle. *)

(* open Core.Std *)
open Async.Std
open Async_extended.Std
open Qtest_lib.Std

let test_coalesce () =
  let throttle = Coalesced_throttle.create () in
  let completed = ref "" in
  let job name ~maybe_abort =
    maybe_abort ~continue:(fun () ->
      completed := !completed ^ name;
      return name)
  in
  Coalesced_throttle.enqueue throttle (job "A")
  >>= fun name ->
    assert_string_equal "A" name;
    Coalesced_throttle.enqueue throttle (job "B")
  >>= fun name ->
    assert_string_equal "B" name;
    assert_string_equal "AB" !completed;
    completed := "";
    let a_job = Coalesced_throttle.enqueue throttle (job "A") in
    let b_job = Coalesced_throttle.enqueue throttle (job "B") in
    a_job
  >>= fun name ->
    assert_string_equal "B" name;
    assert_string_equal "B" !completed;
    b_job
  >>| fun name ->
    assert_string_equal "B" name;
    assert_string_equal "B" !completed

let double_maybe_abort () =
  let throttle = Coalesced_throttle.create () in
  let job ~maybe_abort =
    (* This caused an Ivar.fill on full ivar in original implementation. *)
    maybe_abort ~continue:(fun () ->
      maybe_abort ~continue:(fun () ->
        return ()))
  in
  Coalesced_throttle.enqueue throttle job

let tests = [
  "Coalesced_throttle_test.test_coalesce", test_coalesce;
  "Coalesced_throttle_test.double_maybe_abort", double_maybe_abort;
  ]
