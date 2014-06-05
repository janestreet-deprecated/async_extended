open Core.Std
open Async.Std

type 'a t = {
  mutable data: 'a list;
  mutable new_data: unit Ivar.t;
  msg_to_sexp : ('a -> Sexp.t) option;
}

module Filter = struct
  type t 'a 'b = {
    name   : string;
    select : 'a -> 'b option;
  }

  (* To be ignored when composing filter names *)
  let synthetic = "<synthetic>"

  (* compose name, dropping synthetic names *)
  let compose_name ~sep n1 n2 =
    if n1 = synthetic then
      n2
    else if n2 = synthetic then
      n1
    else
      sprintf "(%s %s %s)" n1 sep n2
  ;;

  let arr f = {
    name = synthetic;
    select = fun x -> Some (f x);
  }

  let ( &&& ) f1 f2 =
    { name = compose_name ~sep:"&&&" f1.name f2.name;
      select = fun x ->
        match (f1.select x, f2.select x) with
        | (Some v1, Some v2) -> Some (v1, v2)
        | _ -> None
    }

  let ( >>> ) f1 f2 =
    { name = compose_name ~sep:">>>" f1.name f2.name;
      select = fun x -> Option.find_map (f1.select x) ~f:f2.select}
end

let create_gen ?to_sexp iter =
  let t = { data = []; new_data = Ivar.create (); msg_to_sexp = to_sexp } in
  don't_wait_for
    (iter (fun m ->
       t.data <- m :: t.data;
       Ivar.fill t.new_data ();
       t.new_data <- Ivar.create ();
       Deferred.unit
     ));
  t

let of_stream ?to_sexp s = create_gen ?to_sexp (fun f -> Stream.iter' s ~f)
let of_pipe   ?to_sexp p = create_gen ?to_sexp (fun f -> Pipe.iter    p ~f)

let create ?to_sexp next =
  let rec iter write =
    next ()
    >>= function
      | None -> Deferred.unit
      | Some a -> write a >>= fun () -> iter write
  in
  create_gen ?to_sexp iter

let new_data t = Ivar.read t.new_data

let describe_in_sexp t =
  match t.msg_to_sexp with
  | Some msg_to_sexp ->
    let msgs = List.sexp_of_t msg_to_sexp t.data in
    Sexp.List [Sexp.Atom "Current messages"; msgs]
  | None ->
    let n = List.length t.data in
    let tip =
      if n > 0 then
        " (create Mailbox with ~to_sexp to get a better error)"
      else
        ""
    in
    Sexp.Atom (sprintf "There are %d messages%S" n tip)

let describe t = Sexp.to_string_hum (describe_in_sexp t)

module Timed_out_waiting_for = struct
  type t =
    { filter : string;
      debug : string sexp_option;
      mailbox : Sexp.t;
    } with sexp
end

exception Timed_out_waiting_for of Timed_out_waiting_for.t with sexp

let receive ?debug ?(timeout = Clock.after (sec 10.)) t
      ~filter:{Filter. name = filter_name; select = filter} ~postcond =
  Deferred.create (fun ivar ->
    let rec loop () =
      let (res, remains) =
        List.partition_map t.data ~f:(fun m ->
          match filter m with
          | None -> `Snd m
          | Some x -> `Fst x)
      in
      if postcond res then begin
        t.data <- remains;
        Ivar.fill ivar (List.rev res)
      end else begin
        upon
          (choose [
             choice timeout (fun () -> `Timeout);
             choice (new_data t) (fun () -> `Loop);
           ])
          (function
            | `Timeout ->
              raise (
                Timed_out_waiting_for
                  { Timed_out_waiting_for.
                    filter = filter_name;
                    debug;
                    mailbox = describe_in_sexp t;
                  })
            | `Loop -> loop ())
      end
    in
    loop ())

module Matched_more_than_expected = struct
  type t =
    { filter : string;
      debug : string sexp_option;
      expected : int;
      received : int;
      mailbox : Sexp.t;
    } with sexp
end

exception Matched_more_than_expected of Matched_more_than_expected.t with sexp

let many ?debug ?timeout t n f =
  receive ?debug ?timeout t ~filter:f ~postcond:(fun l ->
    let n_received = List.length l in
    if n_received > n then
      raise (
        Matched_more_than_expected
          { Matched_more_than_expected.
            filter = f.Filter.name;
            debug;
            expected = n;
            received = n_received;
            mailbox = describe_in_sexp t;
          })
    else
      n_received = n)

let two ?debug ?timeout t f =
  many ?debug ?timeout t 2 f
  >>| function
    | [a; b] -> (a, b)
    | _ -> failwith "mailbox.ml bug"

let one ?debug ?timeout t f =
  many ?debug ?timeout t 1 f >>| List.hd_exn

let not_empty ?debug ?timeout t f =
  receive ?debug ?timeout t ~filter:f ~postcond:(Fn.non List.is_empty)

let peek t {Filter.select = f; _} =
  List.map t.data ~f |! List.filter_map ~f:Fn.id |! List.rev

let zero ?(debug = "") t f =
  match peek t f with
  | [] -> ()
  | _ -> failwithf "Mailbox: Not Zero. Filter '%s'. Debug: '%s'." f.Filter.name debug ()

let clear t =
  t.data <- []

let check_clear t =
  if List.is_empty t.data then
    Ok ()
  else
    Or_error.tag (Or_error.error_string (describe t)) "Unconsumed msgs"

