open! Core
open! Async.Std

include module type of Async.Std.Shutdown

(** [deprecated_shutdown_and_raise ?force status] initiates shutdown and immediately
    raises. *)
val deprecated_shutdown_and_raise : ?force:unit Deferred.t -> int -> never_returns
