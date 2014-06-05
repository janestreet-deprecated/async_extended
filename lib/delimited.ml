open Core.Std
open Async.Std
module Csv_writer = Core_extended.Csv_writer

(* the maximum read/write I managed to get off of a socket or disk was 65k *)
let buffer_size = 10 * 65 * 1024

module Header = struct
  type t = [
    | `No
    | `Yes
    | `Limit of string list
    | `Replace of string list
    | `Transform of (string list -> string list)
    | `Add of string list ]
end

module Row = struct
  module Table = String.Table

  type t =
    {
      header_index : int Table.t;
      data         : string array;
      size         : int;
    }

  let is_empty t =
    let rec loop i =
      if i >= Array.length t.data then true
      else if String.length (String.strip t.data.(i)) > 0 then false
      else loop (i + 1)
    in
    loop 0
  ;;

  let sexp_of_t t =
    let index_table =
      Int.Table.of_alist_exn
        (List.map (Table.to_alist t.header_index) ~f:(fun (k,d) -> (d,k)))
    in
    Sexp.List (Array.to_list (Array.mapi t.data ~f:(fun i v ->
      let k,v =
        match Int.Table.find index_table i with
        | None -> Int.to_string i, v
        | Some k -> k, v
      in
      Sexp.List [Sexp.Atom k; Sexp.Atom v])))
  ;;

  let fold t ~init ~f =
    Hashtbl.fold t.header_index ~init ~f:(fun ~key:header ~data:i acc ->
      f acc ~header ~data:t.data.(i))
  ;;

  let iter t ~f =
    let f () ~header ~data = f ~header ~data in
    fold t ~init:() ~f
  ;;

  let to_string t = Sexp.to_string_hum (sexp_of_t t)

  let index_exn t header =
    try
      Table.find_exn t.header_index header
    with
    | _ -> failwithf "header \"%s\" not found" header ()
  ;;

  let get_exn_p t header here =
    let i = index_exn t header in
    try
      t.data.(i)
    with
    | _ ->
      Error.failwithp here "header exists in file but not for this row"
        (`header header, `row t)
        <:sexp_of< [`header of string] * [`row of t] >>
  ;;

  let get_exn t header = get_exn_p t header _here_

  let get_conv_exn t header here conv =
    let v = get_exn_p t header here in
    try
      conv v
    with
    | exn ->
      Error.failwithp here "failed to parse"
        (`header header, `row t, `exn exn)
        <:sexp_of< [`header of string] * [`row of t] * [`exn of Exn.t] >>
  ;;

  let get t header =
    try
      Some (get_exn t header)
    with
    | _ -> None
  ;;

  let nth_exn t i = t.data.(i)

  (*
  let set_exn t ~header value =
    let i = index_exn t header in
    try t.data.(i) <- value
    with
    | _ ->
      failwithf "header \"%s\" exists in file but not for row %s"
        header (String.concat ~sep:"," (Array.to_list t.data)) ()
  ;;

  let set t ~header value =
    Result.try_with (fun () -> t.data.(index_exn t header) <- value)
  ;;
  *)

  let nth t i =
    try
      Some (nth_exn t i)
    with
    | _ -> None
  ;;

  let create header_index row_queue =
    let data = Array.create ~len:(Queue.length row_queue) "" in
    let size = ref 0 in
    let i = ref 0 in
    Queue.iter row_queue ~f:(fun col ->
      data.(!i) <- col;
      size := !size + String.length col;
      i := !i + 1);
    {
      header_index = header_index;
      data = data;
      size = !size;
    }
  ;;

  let to_list t  = Array.to_list t.data
  let to_array t = t.data

  let headers t : int String.Table.t = t.header_index

  let size t     = t.size
end

type ('a,'b) reader =
       ?strip:bool
    -> ?skip_lines:int
    -> ?on_parse_error:[`Raise | `Handle of (string Queue.t -> exn -> [`Continue | `Finish])]
    -> header:'a
    -> 'b

let drop_lines r lines =
  let rec loop n =
    if n = 0
    then Deferred.unit
    else begin
      Reader.read_line r
      >>= function
      | `Ok _ -> loop (n - 1)
      | `Eof  -> failwithf "file has fewer than %i lines" lines ()
    end
  in
  loop lines
;;

let strip_buffer buf =
  let len = Buffer.length buf in
  let rec first_non_space n =
    if n >= len then None
    else if Buffer.nth buf n <> ' ' then Some n
    else first_non_space (n + 1)
  in
  let rec last_non_space n =
    if n < 0 then None
    else if Buffer.nth buf n <> ' ' then Some n
    else last_non_space (n - 1)
  in
  match first_non_space 0 with
  | None   -> ""
  | Some s ->
    match last_non_space (len - 1) with
    | None   -> assert false
    | Some e -> Buffer.sub buf s (e - s + 1)
;;

let make_emit_field ~strip current_row field =
  (fun () ->
    Queue.enqueue current_row
      (if strip then strip_buffer field
      else Buffer.contents field);
    Buffer.clear field)
;;

let set_headers header_index headers =
  List.iteri headers ~f:(fun i h ->
    match Hashtbl.find header_index h with
    | None -> Hashtbl.replace header_index ~key:h ~data:i
    | Some other_i ->
      failwithf "header %s duplicated at position %i and %i" h other_i i ())
;;

let make_emit_row current_row row_queue header ~lineno =
  let module Table = String.Table in
  let header_index =
    match (header : Header.t) with
    | `No | `Yes | `Limit _ | `Transform _ -> Table.create () ~size:1
    | `Replace headers | `Add headers ->
      Table.of_alist_exn (List.mapi headers ~f:(fun i s -> (s,i)))
  in
  let header_processed =
    ref (match header with
    | `No | `Add _ -> true
    | `Limit _ | `Replace _ | `Transform _ | `Yes -> false)
  in
  (fun () ->
    if not !header_processed then begin
      header_processed := true;
      match header with
      | `No | `Add _ -> assert false
      | `Limit at_least ->
        let headers = Queue.to_list current_row in
        List.iter at_least ~f:(fun must_exist ->
          match List.findi headers ~f:(fun _ h -> h = must_exist) with
          | None ->
            failwithf "The required header '%s' was not found in '%s' (lineno=%d)"
              must_exist (String.concat ~sep:"," headers) (!lineno) ()
          | Some (i, _) ->
            Hashtbl.replace header_index ~key:must_exist ~data:i)
      | `Replace _new_headers -> ()  (* already set above *)
      | `Transform f ->
        set_headers header_index (f (Queue.to_list current_row))
      | `Yes -> set_headers header_index (Queue.to_list current_row)
    end else begin
      Queue.enqueue row_queue (Row.create header_index current_row)
    end;
    lineno := !lineno + 1;
    Queue.clear current_row)
;;

let of_reader
    ?(strip=false)
    ?(skip_lines=0)
    ?(on_parse_error=`Raise)
    ~header
    ?(quote='\\')
    ~sep
    reader =
  let module Table = String.Table in
  assert (quote <> sep);
  let lineno        = ref 1 in
  let pipe_r,pipe_w = Pipe.create () in
  let buffer        = String.create buffer_size in
  let field         = Buffer.create 1 in
  let quoted        = ref false in
  let current_row   = Queue.create () in
  let row_queue     = Queue.create () in
  let emit_field    = make_emit_field ~strip current_row field in
  let emit_row      = make_emit_row current_row row_queue header ~lineno in
  let flush_rows () = Pipe.write' pipe_w row_queue in
  let prev_was_cr   = ref false in
  let emit_pending_cr () =
    if !prev_was_cr then begin
      Buffer.add_char field '\r';
      prev_was_cr := false
    end
  in
  let add_char c =
    (* delay adding '\r' characters until we know that the next character is
       not '\n' *)
    emit_pending_cr ();
    if c = '\r' then
      prev_was_cr := true
    else
      Buffer.add_char field c
  in
  let close () =
    don't_wait_for (flush_rows ());
    don't_wait_for (Reader.close reader);
    Pipe.close pipe_w
  in
  let rec loop () =
    Reader.read reader buffer >>> function
    | `Eof ->
      if Queue.length current_row <> 0 then begin
        emit_field ();
        emit_row ();
      end;
      close ()
    | `Ok n ->
      let res =
        Result.try_with (fun () ->
          for i = 0 to n - 1 do
            let c = buffer.[i] in
            if c = '\n' then begin
              prev_was_cr := false;
              if !quoted then begin
                close ();
                failwithf "escape character found at the end of a line (lineno=%d)"
                  (!lineno) ()
              end else begin
                emit_field ();
                emit_row ()
              end
            end else if !quoted then begin
              quoted := false;
              add_char c
            end else if c = sep then begin emit_pending_cr (); emit_field () end
            else if c = quote then begin emit_pending_cr (); quoted := true end
            else add_char c
          done)
      in
      flush_rows ()
      >>> fun () ->
      match res with
      | Ok ()   -> loop ()
      | Error e ->
        match on_parse_error with
        | `Raise    -> raise e
        | `Handle f ->
          emit_field ();
          match f current_row e with
          | `Continue -> loop ()
          | `Finish   -> close ()
  in
  upon (drop_lines reader skip_lines) loop;
  pipe_r
;;

let create_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep filename =
  Reader.open_file filename >>| fun r ->
  of_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep r
;;

module Csv = struct
  module State = struct
    type t =
      | StartField
      | InUnquotedField
      | InQuotedField
      | InQuotedFieldAfterQuote
  end
  open State

  let default_separator = ','

  (* row up to the error, and the field with the error up to the point of failure *)
  exception Bad_csv_formatting of string list * string

  let of_reader
      ?(strip=false)
      ?(skip_lines=0)
      ?(on_parse_error=`Raise)
      ~header
      ?sep:(separator=default_separator)
      reader =
    let lineno        = ref 1 in
    let pipe_r,pipe_w = Pipe.create () in
    let buffer        = String.create buffer_size in
    let state         = ref StartField in
    let field         = Buffer.create 1 in
    let current_row   = Queue.create () in
    let row_queue     = Queue.create () in
    let emit_field    = make_emit_field ~strip current_row field in
    let emit_row      = make_emit_row current_row row_queue header ~lineno in
    let flush_rows () =
      Pipe.write' pipe_w row_queue >>| fun () ->
      Queue.clear row_queue
    in
    let close () =
      don't_wait_for (flush_rows ());
      don't_wait_for (Reader.close reader);
      Pipe.close pipe_w;
    in
    let rec loop () =
      Reader.read reader buffer >>> function
      | `Eof ->
        begin match !state with
        | StartField ->
          if Queue.length current_row <> 0 then begin
            emit_field ();
            emit_row ()
          end;
          close ()
        | InUnquotedField
        | InQuotedFieldAfterQuote ->
          emit_field ();
          emit_row ();
          close ();
        | InQuotedField ->
          close ();
          raise (Bad_csv_formatting (Queue.to_list current_row, Buffer.contents field))
        end
      | `Ok n ->
        let res =
          Result.try_with (fun () ->
            for i = 0 to n - 1 do
              let c = buffer.[i] in
              if c <> '\r' then
                match !state with
                | StartField ->
                  if      c = '\"'      then state := InQuotedField
                  else if c = separator then emit_field ()
                  else if c = '\n'      then (emit_field (); emit_row ())
                  else begin
                    Buffer.add_char field c;
                    state := InUnquotedField
                  end
                | InUnquotedField ->
                  begin
                    if c = separator then
                      (emit_field (); state := StartField)
                    else if c = '\n' then (
                      emit_field ();
                      emit_row ();
                      state := StartField)
                    else Buffer.add_char field c
                  end
                | InQuotedField ->
                  if c = '\"' then
                    state := InQuotedFieldAfterQuote
                  else
                    Buffer.add_char field c
                | InQuotedFieldAfterQuote ->
                  if c = '\"' then ( (* doubled quote *)
                    Buffer.add_char field c;
                    state := InQuotedField)
                  else if c = '0' then (
                    Buffer.add_char field '\000';
                    state := InQuotedField)
                  else if c = separator then (
                    emit_field ();
                    state := StartField)
                  else if c = '\n' then (
                    emit_field ();
                    emit_row ();
                    state := StartField)
                  else if Char.is_whitespace c then ()
                  else
                    failwithf "InQuotedFieldAfterQuote looking at '%c' (lineno=%d)"
                      c (!lineno) ()
            done)
        in
        flush_rows ()
        >>> (fun () ->
        match res with
        | Ok ()   -> loop ()
        | Error e ->
          match on_parse_error with
          | `Raise    -> raise e
          | `Handle f ->
            emit_field ();
            match f current_row e with
            | `Continue -> loop ()
            | `Finish   -> close ())
    in
    upon (drop_lines reader skip_lines) loop;
    pipe_r
  ;;

  let create_reader ?strip ?skip_lines ?on_parse_error ~header ?sep filename =
    Reader.open_file filename >>| fun r ->
    of_reader ?strip ?skip_lines ?on_parse_error ~header ?sep r
  ;;

  let write_field w field = Writer.write w (Csv_writer.maybe_escape_field field)

  let rec write_line ~sep w line =
    match line with
    | [] -> Writer.write w "\r\n"
    | [field] ->
      write_field w field;
      write_line ~sep w []
    | field :: rest ->
      write_field w field;
      Writer.write_char w sep;
      write_line ~sep w rest
  ;;

  let of_writer ?(sep=',') writer =
    let pipe_r, pipe_w = Pipe.create () in
    don't_wait_for (Writer.transfer writer pipe_r (write_line ~sep writer));
    pipe_w
  ;;

  let create_writer ?sep filename =
    Writer.open_file filename >>| fun w ->
    of_writer ?sep w
  ;;
end

module Positional = struct
  type header = (string * int * int) list

  let process_header header ~strict =
    let header = List.sort header ~cmp:(fun (_,a,_) (_,b,_) -> Int.compare a b) in
    let header_index = String.Table.create () in
    let col2str (name, pos, len) =
      sprintf "(name: %s, start:%i, length:%i)" name pos len
    in
    let rec loop i l =
      match l with
      | [] -> Ok (header,header_index)
      | [(name, _, _)] ->
        begin match Hashtbl.add header_index ~key:name ~data:i with
        | `Ok        -> Ok (header,header_index)
        | `Duplicate -> Or_error.error_string ("Duplicate column name: "^name)
        end
      | (name1, pos1, len1) :: (name2, pos2, len2) :: l ->
        if pos1 + len1 > pos2 then
          Or_error.error_string ("Overlapping columns :"
                                 ^ (col2str (name1, pos1, len1))
                                 ^ (col2str (name2, pos2, len2)))
        else if strict && pos1 + len1 <> pos2 then
          Or_error.error_string ("Gap between columns :"
                                 ^ (col2str (name1, pos1, len1))
                                 ^ (col2str (name2, pos2, len2)))
        else
          match Hashtbl.add header_index ~key:name1 ~data:i with
          | `Ok        -> loop (i+1) ((name2, pos2, len2) :: l)
          | `Duplicate -> Or_error.error_string ("Duplicate column name: "^name1)
    in
    loop 0 header
  ;;

  let of_reader
      ?(strip=false)
      ?(skip_lines=0)
      ?(on_parse_error=`Raise)
      ~header
      ?(strict=true)
      reader =
    match process_header header ~strict with
    | Error e -> Error e
    | Ok (header, header_index) ->
      let pipe_r,pipe_w = Pipe.create () in
      let n_cols = List.length header in
      let parse_line line =
        let data = Array.create ~len:n_cols "" in
        try
          List.iter header ~f:(fun (name, pos,len) ->
            let i = Hashtbl.find_exn header_index name in
            if strip then
              data.(i) <- String.strip (String.sub line ~pos ~len)
            else
              data.(i) <- String.sub line ~pos ~len
          );
          Ok { Row.header_index
             ; data
             ; size = String.length line
             }
        with
        | e -> Error e
      in
      let close () =
        don't_wait_for (Reader.close reader);
        Pipe.close pipe_w;
      in
      let rec loop () =
        Reader.read_line reader
        >>> function
        | `Eof -> close ()
        | `Ok line ->
          match parse_line line with
          | Ok row -> Pipe.write pipe_w row >>> loop
          | Error e ->
            match on_parse_error with
            | `Raise -> close (); raise e
            | `Handle f ->
              match f (Queue.create ()) e with
              | `Continue -> loop ()
              | `Finish -> close ()
      in
      upon (drop_lines reader skip_lines) loop;
      Ok pipe_r
  ;;

  let create_reader ?strip ?skip_lines ?on_parse_error ~header ?strict filename =
    Reader.open_file filename >>| fun r ->
    of_reader ?strip ?skip_lines ?on_parse_error ~header ?strict r
  ;;

  let rec write_line w header next_pos line =
    match header, line with
    | [], [] -> Writer.write w "\r\n"
    | (_,pos,len)::header, field::line ->
      let pre_fill =
        if next_pos < pos then
          String.init (pos - next_pos) ~f:(const ' ')
        else
          ""
      in
      let fill =
        if String.length field < len then
          String.init (len - String.length field) ~f:(const ' ')
        else
          ""
      in
      Writer.write w (pre_fill ^ field ^ fill);
      write_line w header (pos + len) line
    | [], _ -> raise (Invalid_argument "Too many fields given")
    | _, [] -> raise (Invalid_argument "Too few fields given")
  ;;

  let of_writer writer ?(strict=true) header =
    match process_header header ~strict with
    | Error e -> Error e
    | Ok (header, _) ->
      let pipe_r, pipe_w = Pipe.create () in
      don't_wait_for (Pipe.iter_without_pushback pipe_r ~f:(write_line writer header 0));
      Ok pipe_w
  ;;

  let create_writer filename ?strict header =
    Writer.open_file filename
    >>| fun w ->
    of_writer w header ?strict
  ;;
end
