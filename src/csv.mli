open! Core.Std
open! Async.Std

(** All readers defined below will raise if they encounter unparsable content. *)

module Header : Delimited_intf.Header

(** This provides an applicative interface for constructing values from a csv file.
    An ['a Builder.t] represents a method for creating a value of type ['a] from a
    row of a csv file.
*)
module Builder : sig
  type 'a t

  include Applicative.S with type 'a t := 'a t

  module Let_syntax : sig
    module Let_syntax : sig
      include Applicative.S with type 'a t := 'a t
      module Open_on_rhs : sig
        val at_index : int -> f:(string -> 'a) -> 'a t
        val at_header : string -> f:(string -> 'a) -> 'a t
      end
    end
  end

  val at_index : int -> f:(string -> 'a) -> 'a t
  val at_header : string -> f:(string -> 'a) -> 'a t
end

(** [of_reader ?strip ?skip_lines ?sep ~init ~f r] produces a value by folding
    over a csv document read from [r].

    If [strip] is true, leading and trailing whitespace is stripped from each field.
    Default value is false.

    If [skip_lines] > 0, that many lines are skipped at the start of the input.
    Note that this skips lines without doing any CSV parsing of the lines being skipped,
    so newlines within a quoted field are treated identically to newlines outside a
    quoted field.
    Default value is 0.

    [sep] is the character that separates fields within a row.
    Default value is ','
*)
val fold_reader :
  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?header:Header.t
  -> builder:'a Builder.t
  -> init:'b
  -> f:('b -> 'a -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** [of_reader' ?strip ?skip_lines ?sep ~init ~f r] works similarly to
    [of_reader], except for the [f] argument. [of_reader'] runs [f] on batches
    of [Row.t]s rather than running [f] on each individual row.
*)
val fold_reader' :
  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?header:Header.t
  -> builder:'a Builder.t
  -> init:'b
  -> f:('b -> 'a Queue.t -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

val fold_reader_without_pushback :
  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?header:Header.t
  -> builder:'a Builder.t
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> Reader.t
  -> 'b Deferred.t

val fold_reader_to_pipe :
  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?header:Header.t
  -> builder:'a Builder.t
  -> Reader.t
  -> 'a Pipe.Reader.t

val fold_string :
  ?strip:bool
  -> ?sep:char
  -> ?header:Header.t
  -> builder:'a Builder.t
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> string
  -> 'b

module Replace_delimited_csv : sig

  module Delimited_deprecated = Delimited

  module Delimited : sig

    module Row : sig
      include Delimited_intf.Row

      val builder : t Builder.t
    end

    include Delimited_intf.S with module Row := Row

  end

end
