open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std

module Any_error = Async_extended.Std.Any_error

let test () =
  let after_sec = Clock.after (sec 1.) >>| fun () -> Ok () in
  Any_error.all [return (Ok 1); return (Ok 2); return (Ok 3)]
  >>= fun xs ->
  <:test_result< (int list, unit) Result.t >> ~expect:(Ok [1;2;3]) xs;
  Any_error.both (return (Error ())) after_sec
  >>= function
  | Ok ((), ()) -> failwith "unexpected Ok"
  | Error () ->
  Any_error.both after_sec (return (Error ()))
  >>= function
  | Ok ((), ()) -> failwith "unexpected Ok"
  | Error () ->
    Any_error.all_ignore [after_sec; return (Error ()); after_sec]
  >>= function
  | Ok () -> failwith "unexpected Ok"
  | Error () ->
    if Deferred.is_determined after_sec
    then failwith "Either this test is failing or just running very slowly."
    else Deferred.unit

let tests = [ "Any_error.test", test ]
