open! Core
open! Async

include module type of Async.Shutdown

(** [deprecated_shutdown_and_raise ?force status] initiates shutdown and immediately
    raises. *)
val deprecated_shutdown_and_raise : ?force:unit Deferred.t -> int -> never_returns
