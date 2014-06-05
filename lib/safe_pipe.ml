open Core.Std
open Async.Std

include Pipe

(** Take all elements from [queue], process them with [f], stuff into another queue, stop
    at the first error. Put the error in the queue as well *)
let queue_transfer_till_error ~f ~input queue =
  let outqueue = Queue.create () in
  with_return (fun r ->
    let handle_error err =
      Pipe.close_read input;
      Queue.enqueue outqueue err;
      r.return ()
    in
    Queue.iter queue ~f:(function
    | Error _ as err -> handle_error err
    | Ok x ->
      try
        f outqueue x;
      with | exn ->
        handle_error (Or_error.of_exn ~backtrace:`Get exn)
    ));
  outqueue

(** Helper functions that would go over queue until Error is encountered, then stop
    traversal and close supplied input pipe *)
let queue_map_till_error ~f ~input queue =
  queue_transfer_till_error
    queue
    ~input
    ~f:(fun outqueue x -> Queue.enqueue outqueue (Ok (f x)))
  |! Deferred.return

let queue_filter_map_till_error ~f ~input  queue =
  queue_transfer_till_error
    queue
    ~input
    ~f:(fun outqueue x ->
      match f x with
      | Some y -> Queue.enqueue outqueue (Ok y)
      | None -> ())
  |! Deferred.return


let wrap_ok input = Pipe.map input ~f:(fun x -> Ok x)

let transfer input output ~f  =
  Pipe.transfer' input output
    ~f:(queue_map_till_error ~f ~input)

let map input ~f =
  Pipe.map' input
    ~f:(queue_map_till_error ~f ~input)

let filter_map input ~f =
  Pipe.map' input
    ~f:(queue_filter_map_till_error ~f ~input)

let filter input ~f =
  Pipe.map' input
    ~f:(queue_filter_map_till_error ~input
          ~f:(fun x -> if f x then Some x else None))
