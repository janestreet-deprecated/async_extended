open Core.Std
module Process_in_this_dir = Process
open Async.Std
module Process = Process_in_this_dir

include Reader

let input_sexps reader =
  Deferred.create (fun ivar ->
    let rec loop accum =
      upon (Reader.read_sexp reader)
        (function
        | `Ok s -> loop (s :: accum)
        | `Eof -> Ivar.fill ivar accum
        )
    in
    loop []
  )

let gzip_is_ok = function
  | Error (`Signal s) when s = Signal.pipe -> true
  | status -> Result.is_ok status

let with_input_from_process ~prog ~args f =
  let my_is_ok status =
    match status with
    | Error (`Signal s) when s = Signal.pipe -> true
    | _ -> Result.is_ok status
  in
  Process.open_in ~is_ok:my_is_ok ~prog ~args ()
  >>= (fun {Process.Output.stdout = stdout; stderr = stderr} ->
    Monitor.protect ~finally:(fun () -> Reader.close stderr)
      (fun () ->
        Monitor.protect ~finally:(fun () -> Reader.close stdout)
          (fun () ->  f stdout)
        >>= fun res ->
        Reader.contents stderr
        >>| fun err -> (res, err))
    >>| fun (res, err) ->
    match err with
    | "" -> res
    | str -> failwithf "with_input_from_process: failed with %s" str ())

let with_gzip_file file ~f =
  with_input_from_process ~prog:"gunzip" ~args:["--to-stdout"; file] f

let with_hadoop_gzip_file ~hadoop_file f =
  with_input_from_process ~prog:"bash"
    ~args:["-c"; sprintf "qdfsCmd.exe cat %s | gunzip -c" hadoop_file ] f

let with_xzip_file file ~f =
  with_input_from_process ~prog:"xzcat" ~args:["--stdout"; file] f

let open_gzip_file file =
  Process.open_in ~is_ok:gzip_is_ok ~prog:"gunzip" ~args:["--to-stdout"; file] ()
  >>= fun {Process.Output.stdout = stdout; stderr} ->
  Reader.close stderr
  >>= fun () ->
  return stdout
