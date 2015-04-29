open Core.Std
open Async_extended.Std

val timeout : Time.Span.t -> unit Cml.event

module Branch : sig
  type 'a t
  val (==>) : 'a Cml.event -> ('a -> 'b Cml.t) -> 'b t
  val select : 'a t list -> 'a Cml.t
end

val (|>>|) :
  (cin:'a ->         cout:'b Cml.cout -> unit) ->
  (cin:'b Cml.cin -> cout:'c          -> unit) ->
  (cin:'a ->         cout:'c          -> unit)

val (>>|) :
  (                  cout:'b Cml.cout -> unit) ->
  (cin:'b Cml.cin -> cout:'c          -> unit) ->
  (cin:unit       -> cout:'c          -> unit)

val (|>>) :
  (cin:'a          -> cout:'b Cml.cout -> unit) ->
  (cin:'b Cml.cin                      -> unit) ->
  (cin:'a ->       cout:unit           -> unit)

(*
  echo each entry [v] from [cin] to [cout] with delay [delay v]
  use [accum] to aggregate any additional [v]s that pile up while waiting.
  aggreage with [accum] any [cin]-read values that appear while writing
  to [cout]
*)
val echo :
  accum:('a -> 'a -> 'a)
  -> delay:('a -> Time.Span.t)
  -> cin:'a Cml.cin
  -> cout:'a Cml.cout
  -> unit

