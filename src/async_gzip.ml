open Core.Std
open Async.Std

type t =
  { ic : Gzip.in_channel
  ; thread : In_thread.Helper_thread.t
  }

let open_in name =
  match In_thread.Helper_thread.create () with
  | Error e -> Error.raise e
  | Ok thread ->
    In_thread.run ~thread (fun () -> Gzip.open_in name)
    >>| fun ic ->
    { thread ; ic }
;;

let input_char { ic ; thread } =
  In_thread.run ~thread (fun () -> try `Ok (Gzip.input_char ic) with End_of_file -> `Eof)
;;

let input_byte { ic ; thread } =
  In_thread.run ~thread (fun () -> try `Ok (Gzip.input_byte ic) with End_of_file -> `Eof)
;;

let input { ic ; thread } buf pos len =
  In_thread.run ~thread (fun () -> Gzip.input ic buf pos len)
;;

let close_in { ic ; thread } =
  In_thread.run ~thread (fun () -> Gzip.close_in ic)
;;

let input_line_blocking ?(fix_win_eol = true) ic =
  let buf = Buffer.create 128 in
  let rec loop last_char_slash_r =
    match (try Some (Gzip.input_char ic) with End_of_file -> None) with
    | None ->
        if Buffer.length buf = 0 then `Eof
        else `Ok (Buffer.contents buf)
    | Some c ->
        if last_char_slash_r && Char.(<>) c '\n' then Buffer.add_char buf '\r';
        if Char.(=) c '\n' then begin
          if not fix_win_eol && last_char_slash_r then Buffer.add_char buf '\r';
          `Ok (Buffer.contents buf)
        end else if Char.(=) c '\r' then
          loop true
        else begin
          Buffer.add_char buf c;
          loop false
        end
  in loop false

let input_line ?(fix_win_eol = true) { ic ; thread } =
  In_thread.run ~thread (fun () ->
    input_line_blocking ~fix_win_eol ic
  )
;;

let with_file name ~f =
  open_in name >>= fun t ->
  f t >>= fun res ->
  close_in t >>| fun () ->
  res
;;

let _pipe_of_file name =
  let pipe t =
    let (pipe_r, pipe_w) = Pipe.create () in
    let fill_pipe () =
      let rec read () =
        input_line t
        >>= function
          | `Eof -> return (Pipe.close pipe_w)
          | `Ok line ->
            don't_wait_for (Pipe.write pipe_w line);
            read ()
      in
      read ()
    in
    don't_wait_for (fill_pipe ());
    return pipe_r
  in
  with_file name ~f:pipe
;;

let pipe_of_file filename =
  let pipe_r,pipe_w = Pipe.create () in
  let write_to_pipe () =
    open_in filename
    >>= fun t ->
    let rec write_line () =
      input_line t
      >>= function
      | `Eof ->
        Pipe.close pipe_w;
        close_in t
      | `Ok line ->
        Pipe.write pipe_w line
        >>= write_line
    in write_line ()
  in
  don't_wait_for (write_to_pipe ());
  pipe_r
