open Core.Std
open Async.Std
open Async_extended.Std
open Cml

let timeout span = determined (Clock.after span)

module Branch : sig
  type 'a t
  val (==>) : 'a event -> ('a -> 'b Cml.t) -> 'b t
  val select : 'a t list -> 'a Cml.t
end = struct
  type 'a t = 'a Cml.t Cml.event
  let (==>) = wrap
  let select ts = join (sync (choose ts))
end
open Branch

let (|>>|) f g = (); fun ~cin ~cout ->
  let (inner_cout, inner_cin) = pipe () in
  f ~cin ~cout:inner_cout;
  g ~cin:inner_cin ~cout;
;;

let (>>|) f g = (); fun ~cin:() ~cout ->
  let (inner_cout, inner_cin) = pipe () in
  f ~cout:inner_cout;
  g ~cin:inner_cin ~cout;
;;

let (|>>) f g = (); fun ~cin ~cout:() ->
  let (inner_cout, inner_cin) = pipe () in
  f ~cin ~cout:inner_cout;
  g ~cin:inner_cin;
;;

let echo ~accum ~delay ~cin ~cout =
  let (+) a b = accum a b in
  let rec
    empty () =
      sync (recv cin) >>= full_waiting
  and
    full_waiting v =
      select [
        (timeout (delay v) ==> fun () ->
          full_ready v None);
        (recv cin ==> fun v' ->
          full_waiting (v + v'))
      ]
  and
    full_ready v pending=
      select [
        (send cout v ==> fun () ->
          match pending with
          | None -> empty ()
          | Some v -> full_waiting v);
        (recv cin ==> fun v' ->
          let pending =
            match pending with
            | None -> Some v'
            | Some v'' -> Some (v' + v'')
          in
          full_ready v pending);
      ]
  in
  spawn empty
;;

let () = Random.self_init ()
let delay _ = sec (Random.float 0.001)

let driver ~cout =
  let rec loop i =
    sync (timeout (delay ())) >>= fun () ->
    sync (send cout i) >>= fun () ->
    Print.printf "SENT %i\n%!" i;
    loop (i+1)
  in
  spawn (fun () -> loop 0)
;;

let dump ~cin =
  let rec loop () =
    sync (recv cin) >>= fun i ->
    Print.printf "\t\tRECV %i\n%!" i;
    loop ()
  in
  spawn loop
;;

let main = driver >>| echo ~accum:(+) ~delay |>> dump

let () =
  main ~cin:() ~cout:();
  Stream.iter (Monitor.detach_and_get_error_stream (Monitor.current ())) ~f:(fun error ->
    match Monitor.extract_exn error with
    | Unix.Unix_error (EPIPE, _, _) ->
      Shutdown.shutdown 0 ~force:(after (sec 0.01))
    | _ ->
        Print.eprintf "%s\n%!" (Exn.to_string error);
        Shutdown.shutdown 1);
  never_returns (Scheduler.go ());
;;

