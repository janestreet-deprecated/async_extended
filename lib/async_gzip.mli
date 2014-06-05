open Core.Std
open Async.Std

type t

val open_in : string -> t Deferred.t
val with_file : string -> f:(t -> 'a Deferred.t) -> 'a Deferred.t
val input_char : t -> [> `Eof | `Ok of char ] Deferred.t
val input_byte : t -> [> `Eof | `Ok of int ] Deferred.t
val input : t -> string -> int -> int -> int Deferred.t
val close_in : t -> unit Deferred.t
val input_line : ?fix_win_eol:bool -> t -> [> `Eof | `Ok of string ] Deferred.t
(*added by ngros on 2011-11-29, this is one of my first async modules so let me know if
  you can find something bad with it!*)
val pipe_of_file : string -> string Pipe.Reader.t
