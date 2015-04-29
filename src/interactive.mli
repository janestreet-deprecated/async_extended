open Core.Std
open Async.Std

val interactive : bool ref

val print_string : string -> unit Deferred.t

val print_endline : string -> unit Deferred.t

val printf          : ('r, unit, string, unit Deferred.t) format4 -> 'r

val prints : string -> 'a -> ('a -> Sexp.t) -> unit Deferred.t

val ask_dispatch_gen
  :  f:(string -> ('a, string) Result.t)
  -> string
  -> 'a Deferred.t

module Choice : sig
  type +'a t

  val create : char -> 'a -> string -> 'a t

  val default : 'a t -> 'a t
end

(** [ask_dispatch_gen question choices] displays [question] and gets user input to select
    one of the [choices].  At most once choice can be the [default] choice. *)
val ask_dispatch_with_help
  :  ?show_options:bool  (** default is [true] *)
  -> string
  -> 'a Choice.t list
  -> 'a Deferred.t

val ask_yn
  :  ?default:bool
  -> string
  -> bool Deferred.t

val ask_ynf
  :  ?default:bool
  -> ('a, unit, string, bool Deferred.t) format4
  -> 'a

val show_file
  :  ?msg:string
  -> file:string
  -> unit
  -> unit Deferred.t
