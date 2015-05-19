(** CML-style events for synchronous concurrent programming *)

open Core.Std
open Async.Std

(* type of potentially blocking concurrent computations *)
type 'a t

include Monad with type 'a t := 'a t

(** [spawn f] spawns off a concurrent "thread" of execution *)
val spawn : (unit -> never_returns t) -> unit

(** [abort] kills the current "thread" *)
val abort : never_returns t

(** [block d] waits for a [d] to become determined before continuing with the
    result *)
val block : 'a Deferred.t -> 'a t

(** [run t] is a deferred that becomes determined when the "thread" finishes *)
val run : 'a t -> 'a Deferred.t

(** [memo t] memoizes its value the first time it is evaluated to completion.
    subsequent re-evaluations merely return the previously computed result *)
val memo : 'a t -> 'a t

(** [yield] voluntarily relinquishes control to the scheduler. *)
val yield : unit t

(** [noop = return ()] *)
val noop : unit t

(** synchronization events *)
type 'a event

(** [sync e] waits for an event to happen, returning the value of that event *)
val sync : 'a event -> 'a t

(* one-way synchronous communication channels *)
type 'a cin
type 'a cout
val pipe : unit -> 'a cout * 'a cin
val send : 'a cout -> 'a -> unit event
val recv : 'a cin -> 'a event

(* an immediately synchronized variant of [send] *)
val send_now : 'a cout -> 'a -> unit t

(* an immediately synchronized variant of [recv] *)
val recv_now : 'a cin -> 'a t

(* standard event combinators *)
val never : 'a event
val always : 'a -> 'a event
val choose : 'a event list -> 'a event
val wrap : 'a event -> ('a -> 'b) -> 'b event
(* The following two combinators appeared in the original CML, but they seem
   more complicated to implement.  If we ever need them, we can have a go at
   the implementation. *)
(*
val guard : (unit -> 'a event) -> 'a event
val with_nack : (unit event -> 'a event) -> 'a event
*)

(** [determined d] is the event of the deferred [d] becoming determined *)
val determined : 'a Deferred.t -> 'a event

