open Core.Std
open Async.Std
module Posix_clock = Core_extended.Posix_clock
module Bench = Core_extended.Std.Deprecated_bench

module Int63_arithmetic : sig
  type t = Int63.t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( * ) : t -> t -> t
end = Int63

module Result = Bench.Result
let print = Bench.print

module Verbosity = struct
  type t = [ `High | `Mid | `Low ]

  let threshold : t ref = ref `Low

  let cmp a b =
    match a, b with
    | `Low, `Low -> 0
    | `Low, _ -> -1
    | `Mid, `Low -> 1
    | `Mid, `Mid -> 0
    | `Mid, `High -> -1
    | `High, `High -> 0
    | `High, _ -> 1
  ;;

end

let printout msg_verbosity fmt =
  if Verbosity.cmp !Verbosity.threshold msg_verbosity >= 0
  then printf fmt else ksprintf ignore fmt
;;

let default_clock = `Wall

module Test = struct
  type t =
    { name : string option;
      size : int;
      func : unit -> unit Deferred.t;
    }
  ;;

  let create ?name ?(size = 1) func = { name; size; func }

  let name t = t.name
  let size t = t.size
end

let stabilize_gc () =
  let rec loop failsafe last_heap_live_words =
    if failsafe <= 0 then
      failwith "unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.Gc.Stat.live_words <> last_heap_live_words
    then loop (failsafe - 1) stat.Gc.Stat.live_words
  in
  loop 10 0

let full_major_cost ~now () =
  let count = 10 in
  let s = now () in
  for _i = 1 to count do
    Gc.full_major ();
  done;
  let e = now () in
  Int63_arithmetic.((e - s) / Int63.of_int count)


let repeat n f : unit Deferred.t =
  Deferred.create (fun ivar ->
    let rec loop remaining () =
      if Int.(<=) remaining 0 then Ivar.fill ivar () else
        f () >>> loop (remaining - 1)
    in loop n ())
;;

let find_run_size ~now gettime_cost f : (int * Int63.t) Deferred.t =
  Deferred.create (fun ivar ->
    let rec loop samples =
      let s = now () in
      repeat samples f
      >>> fun () ->
        let e = now () in
        (* we need enough samples so that the gettime_cost is < 1% of the cost
        of the run and we also demand that the total run take at least .5
        seconds *)
        let open Int63_arithmetic in
        let open Int63.Replace_polymorphic_compare in
        if gettime_cost > (e - s) / Int63.of_int 100 || (e - s) < Int63.of_int 50_000_000
        then loop (Int.( * ) samples 2)
        else Ivar.fill ivar (samples, e - s)
    in loop 1)
;;

let gc_minor_allocated gc_stat_s gc_stat_e =
  Int.of_float (gc_stat_e.Gc.Stat.minor_words -. gc_stat_s.Gc.Stat.minor_words)
;;

let gc_major_allocated gc_stat_s gc_stat_e =
  Int.of_float (gc_stat_e.Gc.Stat.major_words -. gc_stat_s.Gc.Stat.major_words)
;;

let gc_promoted gc_stat_s gc_stat_e =
  Int.of_float (gc_stat_e.Gc.Stat.promoted_words -. gc_stat_s.Gc.Stat.promoted_words)
;;

let run_once ~f ~sample_size ~runs ~index : unit Deferred.t =
  let stat_s = Gc.quick_stat () in
  repeat sample_size f
  >>| fun () ->
    let stat_m = Gc.quick_stat () in
    Gc.full_major ();
    let stat_e = Gc.quick_stat () in
    runs.(index) <- {Result.Stat.
      run_cycles = 0;
      compactions     = stat_e.Gc.Stat.compactions - stat_s.Gc.Stat.compactions;
      promoted        = gc_promoted stat_s stat_e / sample_size;
      minor_allocated = gc_minor_allocated stat_m stat_e / sample_size;
      major_allocated = gc_major_allocated stat_m stat_e / sample_size;
    }
;;

let parse_clock maybe_clock =
  let clock = Option.value maybe_clock ~default:default_clock in
  match clock with
  | `Wall -> Posix_clock.Monotonic
  | `Cpu -> Posix_clock.Process_cpu
;;

let default_run_count = 100

let bench_basic =
  let billion  = Int63.of_int 1_000_000_000 in
  let open Core.Std.Result.Monad_infix in
  Posix_clock.gettime           >>= fun gettime ->
  Posix_clock.mean_gettime_cost >>= fun mean_gettime_cost ->
  Posix_clock.min_interval      >>| fun min_interval ->
  let open Deferred.Monad_infix in
  fun ~gc_prefs ~no_compactions ?clock ~trials {Test.name; func = f; size=_ } ->
  let old_gc = Gc.get () in
  begin match gc_prefs with
    | Some prefs -> Gc.set prefs
    | None -> ()
  end;
  let measurement_clock = parse_clock clock in
  let now () = gettime measurement_clock in
  if no_compactions then
    Gc.set { (Gc.get ()) with Gc.Control.max_overhead = 1_000_000 };
  printout `High "calculating cost of timing measurement: %!";
  let gettime_cost =
    mean_gettime_cost ~measure:measurement_clock ~using:Posix_clock.Monotonic
  in
  printout `High "%s ns\n%!" (Int63.to_string gettime_cost);
  printout `High "calculating minimal measurable interval: %!";
  let run_count = match trials with
    | `Auto -> default_run_count
    | `Num n -> n
  in
  let gettime_min_interval = min_interval measurement_clock in
  printout `High "%s ns\n%!" (Int63.to_string gettime_min_interval);
  (* find the number of samples of f needed before gettime cost is < 1% of the total *)
  printout `High "determining number of runs per sample: %!";
  find_run_size ~now gettime_min_interval f
  >>= fun (sample_size, run_time) ->
    printout `High "%i\n%!" sample_size;
    let runs = Array.create ~len:run_count Result.Stat.empty in
    printout `High "stabilizing GC: %!";
    stabilize_gc ();
    printout `High "done\n%!";
    printout `High "calculating the cost of a full major sweep: %!";
    let full_major_cost = full_major_cost ~now () in
    printout `High "%s ns\n%!" (Int63.to_string full_major_cost);
    printout `Mid "running samples for %s (estimated time %s sec)\n%!"
      (Option.value ~default:"(NO NAME)" name)
      (Int63.to_string (Int63_arithmetic.((run_time * Int63.of_int run_count) / billion)));
    Deferred.create (fun ivar ->
      let rec loop remaining f () =
        printout `Mid ".%!";
        if Int.(<) remaining 0 then Ivar.fill ivar () else
          run_once ~f ~sample_size ~runs ~index:remaining
          >>> loop (remaining - 1) f
      in loop (run_count - 1) f ())
  >>= fun () ->
    printout `Mid "\n%!";
    (* keep f from being gc'd by calling f () again *)
    f ()
  >>| fun () ->
    Gc.set old_gc;
    runs
;;

let bench_raw =
  let bench_basic = Or_error.ok_exn bench_basic in
  fun ?verbosity ?gc_prefs ?(no_compactions = false) ?(trials = `Num default_run_count) ?clock tests ->
    Verbosity.threshold := Option.value ~default:`Low verbosity;
    Deferred.List.map tests ~f:(fun test ->
      bench_basic ~gc_prefs ~no_compactions ?clock ~trials test
      >>| fun results -> (test.Test.name, test.Test.size, results))
;;

let bench
    ?(shutdown_after=false) ?limit_width_to ?columns ?display ?verbosity ?gc_prefs
    ?no_compactions ?trials ?clock tests =
  bench_raw ?verbosity ?gc_prefs ?no_compactions ?trials ?clock tests
  >>> fun results ->
  print ?limit_width_to ?columns ?display results;
  if shutdown_after then after (sec 5.) >>> fun () -> Shutdown.shutdown 0
;;
