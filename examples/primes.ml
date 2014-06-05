(* Sieve of Eratosthenes as a process network *)
(* code adapted from Reppy's CML book *)

open Core.Std
open Async_extended.Std
open Async_extended.Std.Cml

let send c v = Cml.sync (Cml.send c v)
let recv c = Cml.sync (Cml.recv c)

let from n =
  let (cout, cin) = Cml.pipe () in
  let rec loop n =
    send cout n >>= fun () -> loop (n+1)
  in
  spawn (fun () -> loop n);
  cin
;;

let filter cin ~f:pred =
  let (cout', cin') = Cml.pipe () in
  let rec loop () =
    recv cin >>= fun v ->
      if pred v then
        send cout' v >>= fun () -> loop ()
      else
        loop ()
  in
  spawn loop;
  cin'
;;

let is_divisible n ~by:d = (n mod d = 0)

let primes () =
  let (cout, cin') = Cml.pipe () in
  let rec loop cin =
    recv cin >>= fun p ->
      send cout p >>= fun () ->
        loop (filter cin ~f:(fun n -> not (is_divisible n ~by:p)))
  in
  spawn (fun () -> loop (from 2));
  cin'
;;

let () =
  let cin = primes () in
  let rec loop () =
    recv cin >>= fun p ->
    print_endline (Int.to_string p);
    loop ()
  in
  spawn loop
;;

let () = never_returns (Async.Std.Scheduler.go ())
