(** The standard semaphore concept, with an async API. *)

open Core.Std
open Async.Std

type t with sexp_of

include Invariant.S with type t := t

(** Create a simple counting semaphore, [create initial_value] is the inital value of the
    semaphore.  Callers to [decr] will wait until the value is positive. *)
val create : int -> t

(** Get the current value of the semaphore *)
val value : t -> int

(** Increment the semaphore and if there are jobs sleeping on [decr] wakeup one of them. *)
val incr : t -> unit

(** Decrement the sempahore.  This blocks if resulting the semaphore value is negative. *)
val decr : t -> unit Deferred.t
