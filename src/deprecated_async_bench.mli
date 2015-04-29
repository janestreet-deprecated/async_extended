open Core.Std
open Async.Std
open Core_extended.Std

module Test : sig
  type t
  val create : ?name:string -> ?size:int -> (unit -> unit Deferred.t) -> t
  val name : t -> string option
  val size : t -> int
end

(**
   @param gc_prefs can be used to change the GC settings during testing
   @param no_compactions can be used to disable GC compactions.

   Both of these are reset after the benchmarking is done and [no_compactions]
   takes precedence over [gc_prefs].

   @param verbosity (default [`Low])
   - [`Low] will print only the output of the benchmarking
   - [`Mid] will additionally print time estimates and a status line
   - [`High] will additionally print information at each step

   @param fast (default false) perform 1/100th as many tests

   @param clock (default [`Wall])
   - [`Wall] will include waiting on I/O or when process is suspended/descheduled
   - [`Cpu] will only count time spent on computations.
*)

(** [bench foo] is equivalent to [print (bench_raw foo)].

    @param shutdown_after (default false) shut down the Async scheduler after
    all tests are run.
*)
val bench :
  ?shutdown_after:bool
  -> (Test.t list -> unit)
  Deprecated_bench.with_benchmark_flags
  Deprecated_bench.with_print_flags

(** Returns a list documenting the runtimes rather than printing to stdout.
    These can be fed to print for results identical to calling bench.
*)
val bench_raw :
  (Test.t list -> Deprecated_bench.Result.t list Deferred.t)
  Deprecated_bench.with_benchmark_flags

val print : (Deprecated_bench.Result.t list -> unit) Deprecated_bench.with_print_flags
