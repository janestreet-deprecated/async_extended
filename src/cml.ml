open Core.Std
open Async.Std

module Unregister : sig
  type t
  val create : (unit -> unit) -> t
  val run : t -> unit
  val noop : t
end = struct
  type t = unit -> unit
  let create t = t
  let run t = t ()
  let noop () = ()
end

type 'a t = ('a -> unit) -> unit
(* the "(); ..." and "let f_a = f a" funny business optimizes for
   currying that happens due to [t] being abstract outside this module *)
let return x = (); fun k -> k x
let bind m f = (); fun k -> m (fun a -> let f_a = f a in f_a k)
let map m ~f = (); fun k -> m (fun x -> k (f x))
let join m   = (); fun k -> m (fun t -> t k)

module X = Monad.Make (struct
  type nonrec 'a t = 'a t
  let return = return
  let bind = bind
  let map = `Custom map
end)
let all_ignore = X.all_ignore
let all        = X.all
let ignore_m     = X.ignore_m
module Monad_infix = X.Monad_infix
include Monad_infix

let memo t =
  let cache = ref (`Empty t) in
  fun k ->
    match !cache with
    | `Full v -> k v
    | `Empty t -> t (fun v -> cache := `Full v; k v)
  (* it seems impossible to cache raised exceptions (like we ask our
     interview candidates to do for a similar problem), due to the
     continuations used here *)
;;

let block d = (); fun k -> d >>> k

let yield = fun k -> Deferred.unit >>> k

let noop = return ()

type 'a base_event = {
  poll : unit -> 'a option;
  register : 'a Handler.t -> commit:(unit -> bool) -> Unregister.t;
    (* the [commit] action is there to unregister any other competing
       events at commit time.  *)
}

(* a composite event happens when any one of the base events does *)
type 'a event = 'a base_event list

let spawn f = Deferred.unit >>> fun () -> f () never_returns

let abort (_k : never_returns -> unit) = ()

let sync es = (); fun k ->
  match List.find_map es ~f:(fun e -> e.poll ()) with
  | Some v -> k v
  | None ->
      let k = Handler.create k in
      let unregisters = ref [] in
      let commit () =
        match !unregisters with
        | [] -> false
        | us ->
          unregisters := [];
          List.iter us ~f:(fun u -> Unregister.run u);
          true
      in
      unregisters := List.map es ~f:(fun e -> e.register ~commit k)
;;

let always v =
  let poll () = Some v in
  let register _ ~commit:_ = Unregister.noop in
  [{poll = poll; register = register}]
;;

let never =
  let poll () = None in
  let register _ ~commit:_ = Unregister.noop in
  [{poll = poll; register = register}]
;;

let choose ess = List.concat ess

let wrap es f =
  List.map es ~f:(fun e -> {
    poll = (fun () -> Option.map ~f (e.poll ()));
    register = (fun h ~commit -> e.register (Handler.prepend h ~f) ~commit);
  })
;;

module Channel = struct

  type ('a, 'b) pending = {
    v : 'a;
    h : 'b Handler.t;
    commit : unit -> bool;
  }

  (* this type can be modified into a "swap" channel by generalizing [unit]
     to ['b].  Then senders would receive a ['b] value and recvers would
     have to send one.  Not sure what such a thing would be good for, though.
  *)
  type 'a t = {
    senders : ('a, unit) pending Bag.t;
    recvers : (unit, 'a) pending Bag.t;
  }

  let create () = {senders = Bag.create (); recvers = Bag.create ()}

  let transmit v ~outgoing ~incoming =
    let poll () =
      Option.map (Bag.remove_one incoming) ~f:(fun susp ->
        assert (susp.commit ());
        Handler.schedule susp.h v;
        susp.v)
    in
    let register h ~commit =
      let susp = {v = v; h = h; commit = commit} in
      let entry = Bag.add outgoing susp in
      Unregister.create (fun () ->
        if not (Bag.is_empty outgoing) then
          Bag.remove outgoing entry)
    in
    [{poll = poll; register = register}]
  ;;

  let send t v = transmit v ~outgoing:t.senders ~incoming:t.recvers
  let recv t = transmit () ~outgoing:t.recvers ~incoming:t.senders

end

type 'a cin = 'a Channel.t
type 'a cout = 'a Channel.t
let pipe () = (let c = Channel.create () in (c, c))

let send = Channel.send
let recv = Channel.recv

let send_now cout v = sync (send cout v)
let recv_now cin = sync (recv cin)

(* conversion between ['a t] and ['a Deferred.t] *)
let run t = Deferred.create (fun ivar -> t (fun a -> Ivar.fill ivar a))

let determined d =
  let poll () = Deferred.peek d in
  let register h ~commit =
    let h = Handler.filter h ~f:(fun _ -> commit ()) in
    Unregister.create (Handler.install h d)
  in
  [{poll = poll; register = register}]
;;
