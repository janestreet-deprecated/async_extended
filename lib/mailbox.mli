open Core.Std
open Async.Std

(** Erlang style mailboxes built on top of async streams *)
type 'a t

module Filter : sig
  type ('a, 'b) t = {
    name   : string;
    select : 'a -> 'b option;
  }

  (* arrows inspired interface for composing filters *)

  (** an always matching filter from a function *)
  val arr : ('a -> 'b) -> ('a, 'b) t

  (** Compose two filters such that both are applied to the same value,
      and their corresponding results are paired in a tuple. This filter
      will fail if any of the two argument filters fails. *)
  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, ('b * 'c)) t

  (** Compose two filters **)
  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
end

val create :
  ?to_sexp:('a -> Sexp.t)
  -> (unit -> 'a option Deferred.t)
  -> 'a t

val of_stream :
  ?to_sexp:('a -> Sexp.t)
  -> 'a Stream.t
  -> 'a t

val of_pipe :
  ?to_sexp:('a -> Sexp.t)
  -> 'a Pipe.Reader.t
  -> 'a t

(** [receive t f] apply [f] to each message in the mailbox, return the list of messages
    for which [f] returned [Some x].  If [f] does not return [Some x] for any available
    messages then wait until one arrives for which it does.  If at any point [timeout]
    becomes determined raise an exception. *)
val receive
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> 'a t
  -> filter: ('a, 'b) Filter.t
  -> postcond:('b list -> bool)
  -> 'b list Deferred.t

(* returns elements from the mailbox in ascending order by arrival time *)
val peek : 'a t -> ('a, 'b) Filter.t -> 'b list

(** [zero t f] asserts that there are exactly zero matching messages. *)
val zero : ?debug:string -> 'a t -> ('a, 'b) Filter.t -> unit

(** [one t f] run receive, asserting that there is exactly one matching message. *)
val one
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> 'a t
  -> ('a, 'b) Filter.t
  -> 'b Deferred.t

(** [two t f] run receive, asserting that there are exactly two matching messages. *)
val two
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> 'a t
  -> ('a, 'b) Filter.t
  -> ('b * 'b) Deferred.t

(** [many t n f] run receive, asserting that there are exactly [n] matching messages *)
val many
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> 'a t
  -> int
  -> ('a, 'b) Filter.t
  -> 'b list Deferred.t

(** [not_empty t f] run receive, asserting that there is at least one matching message. *)
val not_empty
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> 'a t
  -> ('a, 'b) Filter.t
  -> 'b list Deferred.t

(** [clear t] wipes out all previously received messages.

    Immediately after calling [clear t], [zero t f] succeeds for any [f].
*)
val clear : _ t -> unit

(** [check_clear t] - Ok if the mailbox is empty, descriptive error if the mailbox
    has any messages *)
val check_clear : _ t -> unit Or_error.t
