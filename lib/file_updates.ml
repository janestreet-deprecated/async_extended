open Core.Std
open Async.Std

module Update = struct
  type 'a t =
    | Update of 'a
    | Error of ([`Transform | `Read] * exn)
end

let updates ?(poll_interval=sec 5.) filename ~transform =
  let f =
    match transform with
    | `Of_sexp f -> (fun s -> f (Sexp.of_string (String.strip s)))
    | `Of_raw f  -> f
  in
  let r,w = Pipe.create () in
  let rec loop last_mtime =
    let loop new_mtime =
      Clock.with_timeout poll_interval (Pipe.closed r)
      >>> function
      | `Result () -> ()
      | `Timeout   -> loop new_mtime
    in
    let error e = Pipe.write w (Update.Error e) >>> (fun () -> loop last_mtime) in
    try_with (fun () -> Unix.stat filename) 
    >>> function
    | Error e -> error (`Read, e)
    | Ok stat ->
      if Time.(>) stat.Unix.Stats.mtime last_mtime then begin
        try_with (fun () -> Reader.file_contents filename) 
        >>> function
        | Error e -> error (`Read, e)
        | Ok contents -> 
          begin match Result.try_with (fun () -> f contents) with
          | Ok transformed_contents -> 
            Pipe.write w (Update.Update transformed_contents) >>> fun () ->
            loop stat.Unix.Stats.mtime
          | Error e -> error (`Transform, e)
          end
      end else loop last_mtime
  in
  loop Time.epoch;
  r


