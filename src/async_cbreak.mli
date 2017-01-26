open! Core
open! Async.Std

val with_cbreak : f: (unit -> 'a Deferred.t) -> 'a Deferred.t
