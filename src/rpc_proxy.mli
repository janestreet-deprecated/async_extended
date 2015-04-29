open Core.Std
open Async.Std



(* Implements the given Rpc protocol by forwarding queries to the [Rpc.Connection.t]
   obtained by calling the provided function for every query. *)
val proxy : Rpc.Any.t -> Rpc.Connection.t Rpc.Implementation.t
