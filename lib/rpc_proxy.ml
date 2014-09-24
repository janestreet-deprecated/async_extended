open Core.Std
open Async.Std

(* This is just to improve errors *)
let raise_on_closed_connection connection =
  if Rpc.Connection.is_closed connection then
    failwith "rpc proxy connection closed"

let rpc_proxy :
  'q 'r. ('q, 'r) Rpc.Rpc.t -> Rpc.Connection.t Rpc.Implementation.t
  = fun rpc ->
    Rpc.Rpc.implement rpc (fun connection query ->
      raise_on_closed_connection connection;
      Rpc.Rpc.dispatch_exn rpc connection query)

let pipe_proxy :
  'q 'e 'r. ('q, 'e, 'r) Rpc.Pipe_rpc.t -> Rpc.Connection.t Rpc.Implementation.t
  = fun rpc ->
    Rpc.Pipe_rpc.implement rpc
      (fun connection query ~aborted ->
         raise_on_closed_connection connection;
         Rpc.Pipe_rpc.dispatch rpc connection query
         >>= function
         | Error error -> Error.raise error
         | Ok (Error error) -> return (Error error)
         | Ok (Ok (pipe, _id)) ->
           don't_wait_for (
             aborted
             >>| fun () ->
             Pipe.close_read pipe
           );
           return (Ok pipe)
      )

let state_proxy :
  'q 's 'u 'e. ('q, 's, 'u, 'e) Rpc.State_rpc.t -> Rpc.Connection.t Rpc.Implementation.t
  = fun rpc ->
    Rpc.State_rpc.implement rpc
      (fun connection query ~aborted ->
         raise_on_closed_connection connection;
         Rpc.State_rpc.dispatch rpc connection query ~update:(fun s _ -> s)
         >>= function
         | Error error -> Error.raise error
         | Ok (Error error) -> return (Error error)
         | Ok (Ok (state, pipe, _id)) ->
           don't_wait_for (
             aborted >>| fun () -> Pipe.close_read pipe
           );
           let pipe = Pipe.map pipe ~f:(fun (_s, u) -> u) in
           return (Ok (state, pipe))
      )

let proxy : Rpc.Any.t -> _ = function
  | Rpc.Any.Rpc   rpc -> rpc_proxy   rpc
  | Rpc.Any.Pipe  rpc -> pipe_proxy  rpc
  | Rpc.Any.State rpc -> state_proxy rpc
