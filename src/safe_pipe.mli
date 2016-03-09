open! Core.Std
open! Async.Std

open Pipe

(**
  Alternatives to appropriate Pipe.* functions that would catch exceptions raised by
  transformation functions and convert them to Errors on the output pipe, closing it
  afterwards. Contract is that if there is Error it would be the last element on the pipe.

  If die_on_error is set, Shutdown would be called immediately if we encounter an error.
*)

val wrap_ok : 'a Reader.t -> 'a Or_error.t Reader.t

val transfer : 'a Or_error.t Reader.t -> 'b Or_error.t Writer.t -> f:('a -> 'b) -> unit Deferred.t

val map      : 'a Or_error.t Reader.t -> f:('a -> 'b) -> 'b Or_error.t Reader.t

val filter_map : 'a Or_error.t Reader.t -> f:('a -> 'b option) -> 'b Or_error.t Reader.t

val filter     : 'a Or_error.t Reader.t -> f:('a -> bool) -> 'a Or_error.t Reader.t
