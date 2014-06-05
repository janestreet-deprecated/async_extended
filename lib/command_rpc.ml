open Core.Std
open Async.Std

module Command = struct
  module type T = sig
    type query    with of_sexp
    type response with sexp_of
    val rpc : (query, response) Rpc.Rpc.t
    val implementation : query -> response Deferred.t
  end

  module type T_pipe = sig
    type query    with of_sexp
    type response with sexp_of
    type error    with sexp_of
    val rpc : (query, response, error) Rpc.Pipe_rpc.t
    val implementation :
      query
      -> aborted: unit Deferred.t
      -> (response Pipe.Reader.t, error) Result.t Deferred.t
  end

  type t = [ `Plain of (module T) | `Pipe of (module T_pipe) ]

  let menu impls =
    match
      Map.Poly.of_alist
        (List.map impls ~f:(fun impl ->
          match impl with
          | `Plain plain ->
            let module T = (val plain : T) in
            ((Rpc.Rpc.name T.rpc, Rpc.Rpc.version T.rpc), impl)
          | `Pipe pipe ->
            let module T = (val pipe : T_pipe) in
            ((Rpc.Pipe_rpc.name T.rpc, Rpc.Pipe_rpc.version T.rpc), impl)
         ))
    with
    | `Ok map -> map
    | `Duplicate_key (name, version) ->
      failwithf "multiple implementations of rpc (%s %d)" name version ()

  let implementation = function
    | `Plain plain ->
      let module T = (val plain : T) in
      Rpc.Rpc.implement T.rpc (fun () q -> T.implementation q)
    | `Pipe pipe ->
      let module T = (val pipe : T_pipe) in
      Rpc.Pipe_rpc.implement T.rpc (fun () q ~aborted -> T.implementation q ~aborted)

  type call = {
    rpc_name : string;
    version : int;
    query : Sexp.t;
  } with sexp

  let write_sexp w sexp = Writer.write_sexp w sexp; Writer.newline w

  let main impls ~show_menu mode =
    let stdout = Lazy.force Writer.stdout in
    if show_menu then
      let menu_sexp =
        <:sexp_of<(string * int) list>> (Map.keys (menu impls))
      in
      write_sexp stdout menu_sexp;
      return `Success
    else
      let stdin = Lazy.force Reader.stdin in
      match mode with
      | `Bin_prot ->
        begin
          match
            Rpc.Implementations.create
              ~on_unknown_rpc:`Raise
              ~implementations:(Versioned_rpc.Menu.add (List.map ~f:implementation impls))
          with
          | Error (`Duplicate_implementations _) -> return `Failure
          | Ok implementations ->
            Rpc.Connection.server_with_close stdin stdout ~implementations
              ~connection_state:() ~on_handshake_error:`Raise
            >>| fun () ->
            `Success
        end
      | `Sexp ->
        Reader.read_sexp stdin
        >>= function
        | `Eof -> failwith "unexpected EOF on stdin"
        | `Ok sexp ->
          let call = call_of_sexp sexp in
          match Map.find (menu impls) (call.rpc_name, call.version) with
          | None -> failwithf "unimplemented rpc: (%s, %d)" call.rpc_name call.version ()
          | Some impl ->
            match impl with
            | `Plain plain ->
              let module T = (val plain : T) in
              let query = T.query_of_sexp call.query in
              T.implementation query
              >>| fun response ->
              write_sexp stdout (T.sexp_of_response response);
              `Success
            | `Pipe pipe ->
              let module T = (val pipe : T_pipe) in
              let query = T.query_of_sexp call.query in
              T.implementation query ~aborted:(Deferred.never ())
              >>= function
              | Error e ->
                write_sexp stdout (T.sexp_of_error e);
                return `Failure
              | Ok pipe ->
                Pipe.iter pipe ~f:(fun r ->
                  write_sexp stdout (T.sexp_of_response r);
                  Deferred.unit)
                >>| fun () ->
                `Success

  let async_main status_deferred =
    upon status_deferred (fun status ->
      Shutdown.shutdown begin
        match status with
        | `Success -> 0
        | `Failure -> 1
      end);
    never_returns (Scheduler.go ())

  let menu_doc = " dump a sexp representation of the rpc menu"
  let sexp_doc = " speak sexp instead of bin-prot"

  let create ~summary impls =
    Command.basic ~summary
      Command.Spec.(
        empty
        +> flag "-menu" no_arg ~doc:menu_doc
        +> flag "-sexp" no_arg ~doc:sexp_doc)
      (fun show_menu sexp () ->
        async_main (main impls ~show_menu (if sexp then `Sexp else `Bin_prot)))

end

module Connection = struct
  type 'a with_connection_args =
    ?propogate_stderr:bool (* defaults to true *) -> prog:string -> args:string list -> 'a

  let transfer_stderr child_stderr =
    don't_wait_for (Reader.transfer child_stderr (Writer.pipe (Lazy.force Writer.stderr)))

  let reap pid = don't_wait_for (Deferred.ignore (Unix.waitpid pid))

  let connect_gen ~propogate_stderr ~prog ~args f =
    Process.create ~prog ~args ()
    >>=? fun process ->
    if propogate_stderr then
      transfer_stderr (Process.stderr process);
    reap (Process.pid process);
    f ~stdin:(Process.stdin process) ~stdout:(Process.stdout process)
  ;;

  let with_close ?(propogate_stderr=true) ~prog ~args dispatch_queries =
    connect_gen ~propogate_stderr ~prog ~args
      (fun ~stdin ~stdout ->
        Rpc.Connection.with_close
          stdout
          stdin
          ~connection_state:()
          ~on_handshake_error:`Raise
          ~dispatch_queries)
  ;;

  let create ?(propogate_stderr = true) ~prog ~args () =
    connect_gen ~propogate_stderr ~prog ~args
      (fun ~stdin ~stdout ->
        Rpc.Connection.create
          stdout
          stdin
          ~connection_state:()
        >>| Or_error.of_exn_result)
  ;;
end
