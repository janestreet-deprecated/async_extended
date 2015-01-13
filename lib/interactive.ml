open Core.Std
open Async.Std

let interactive = ref Core.Std.Unix.(isatty stdin && isatty stdout)

let print_string string =
  if not !interactive
  then return ()
  else begin
    print_string string;
    Writer.flushed (force Writer.stdout)
  end
;;

let print_endline string = print_string (sprintf "%s\n" string)

let printf fmt = ksprintf print_string fmt

let prints message a sexp_of_a =
  print_endline ((message, a)
                 |> <:sexp_of< string * a >>
                 |> Sexp.to_string_hum)
;;

let read_line () = Reader.read_line (force Reader.stdin)

let read_char () =
  read_line ()
  >>| function
    | `Eof -> failwith "Received EOF.  Exiting..."
    | `Ok str ->
      let len = String.length str in
      if len = 1 then Ok (Some (Char.lowercase str.[0]))
      else if len = 0 then Ok None
      else Error "Expected only one character."
;;

module Choice = struct
  type 'a t = char * 'a * string

  let create char a string = Char.lowercase char, a, string

  let default (char, a, string) = (Char.uppercase char, a, string)
end

let choose_dispatch (type a) ~(dispatch : (char * a) list)
  : char option -> (a, unit) Result.t Deferred.t = function
  | None ->
    begin match List.filter dispatch ~f:(fun (c, _) -> Char.is_uppercase c) with
    | _ :: _ :: _ as l ->
      failwiths "[Interactive.choose_dispatch] supplied multiple defaults"
        (List.map l ~f:fst) <:sexp_of< char list >>
    | [ _, a ] -> return (Ok a)
    | [] ->
      printf "Invalid empty reply.\n"
      >>| fun () ->
      Error ()
    end
  | Some ch ->
    let filter (reply, _) = Char.equal (Char.lowercase reply) (Char.lowercase ch) in
    match List.find dispatch ~f:filter with
    | Some (_, a) -> return (Ok a)
    | None ->
      printf "Invalid reply [%c]\n" ch
      >>| fun () ->
      Error ()
;;

let ask_dispatch_gen ~f question =
  let rec loop () =
    printf "%s: " question
    >>= fun () ->
    read_line ()
    >>= function
    | `Eof -> failwith "Received EOF.  Exiting..."
    | `Ok line ->
      match f line with
      | Ok res -> return res
      | Error msg ->
        printf "%s\n" msg
        >>= fun () ->
        loop ()
  in
  loop ()
;;

let ask_dispatch (type a)
          ?(show_options = true)
          question
          (dispatch : (char * a) list)
          =
  let prompt =
    if not show_options
    then question
    else
      let cs = List.map dispatch ~f:(fun (c, _) -> Char.to_string c) in
      sprintf "%s [%s]" question (String.concat ~sep:"/" cs)
  in
  let rec loop () =
    printf "%s: " prompt
    >>= fun () ->
    read_char ()
    >>= function
    | Error s ->
      printf "Error: %s\n" s
      >>= fun () ->
      loop ()
    | Ok char ->
      choose_dispatch ~dispatch char
      >>= function
      | Error () -> loop ()
      | Ok res -> return res
  in
  loop ()
;;

let ask_dispatch_with_help ?show_options question dispatch =
  let cr_dispatch = List.map dispatch ~f:(fun (char, value, _help) -> char, `Ok value) in
  let cr_dispatch = cr_dispatch @ [('?', `Help)] in
  let print_help () =
    List.iter dispatch ~f:(fun (char, _value, help) ->
      don't_wait_for (printf "%c : %s\n" char help);
    );
    don't_wait_for (printf "? : Print this help\n");
  in
  let rec loop () =
    ask_dispatch question cr_dispatch ?show_options
    >>= function
    | `Ok value -> return value
    | `Help ->
      print_help ();
      loop ()
  in
  loop ()
;;

let ask_yn ?default question =
  let y, n =
    match default with
    | None       -> 'y', 'n'
    | Some true  -> 'Y', 'n'
    | Some false -> 'y', 'N'
  in
  ask_dispatch question
    [ (y, true)
    ; (n, false)
    ]
;;

let ask_ynf ?default question =
  Printf.ksprintf (ask_yn ?default) question
;;

let show_file ?msg ~file () =
  let pager =
    match Sys.getenv "PAGER" with
    (* Make sure -R is passed to 'less' *)
    | Some p when String.is_prefix p ~prefix:"less" -> p ^ " -R"
    | Some p -> p
    | None -> "less -R"
  in
  let q = Filename.quote in
  let cmd =
    match msg with
    | None ->
      sprintf "cat %s | %s" (q file) pager
    | Some msg ->
      sprintf "{ echo %s; cat %s; } | %s" (q msg) (q file) pager
  in
  (* Use 'system cat' rather than printf to handle 'less' *)
  Monitor.protect (fun () -> Unix.system_exn cmd)
    ~finally:(fun () -> Deferred.unit)
;;
