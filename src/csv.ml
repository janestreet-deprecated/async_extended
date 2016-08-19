open Core.Std
open Async.Std
open Int.Replace_polymorphic_compare
module Csv_writer = Core_extended.Csv_writer

(* the maximum read/write I managed to get off of a socket or disk was 65k *)
let buffer_size = 10 * 65 * 1024

module Fast_queue : sig
  type 'a t

  val create   : ?capacity : int -> unit -> 'a t
  val of_list  : 'a list -> 'a t
  val enqueue  : 'a t -> 'a -> unit
  val nth_exn  : 'a t -> int -> 'a
  val clear    : 'a t    -> unit
  val to_list  : 'a t    -> 'a list
  val to_array : 'a t    -> 'a array
  val length   : 'a t    -> int
end = struct
  (* Fast_queue is faster than Core_queue because it doesn't support dequeue, traversal,
     or detection of mutation during traversal. *)
  type 'a t =
    { mutable array : 'a Option_array.t
    ; mutable length : int
    ; mutable capacity : int
    }

  let to_array t =
    Array.init t.length ~f:(fun i -> Option_array.get_some_exn t.array i)

  let create ?(capacity=1) () =
    { array = Option_array.create ~len:capacity
    ; capacity
    ; length = 0
    }

  let set_capacity t desired_capacity =
    let new_capacity = Int.ceil_pow2 (max 1 (max desired_capacity t.length)) in
    if new_capacity <> t.capacity then begin
      let dst = Option_array.create ~len:new_capacity in
      Option_array.unsafe_blit ~len:t.length ~src:t.array ~src_pos:0 ~dst ~dst_pos:0;
      t.array <- dst;
      t.capacity <- new_capacity
    end
  ;;

  let enqueue t x =
    if t.length >= t.capacity
    then set_capacity t (2 * t.length);
    Option_array.unsafe_set_some t.array t.length x;
    t.length <- t.length + 1
  ;;

  let clear t = t.length <- 0

  let nth_exn t n =
    if n < 0 || n >= t.length
    then failwith "Index out of bounds"
    else Option_array.get_some_exn t.array n
  ;;

  let of_list l =
    let t = create ~capacity:(List.length l) () in
    List.iter l ~f:(fun x -> enqueue t x);
    t
  ;;

  let to_list t = List.init t.length ~f:(Option_array.get_some_exn t.array)

  let length t = t.length
end

module Header = struct
  type t = [
    | `Yes
    | `No
    | `Limit of string list
    | `Replace of string list
    | `Transform of (string list -> string list)
    | `Add of string list
  ] [@@deriving sexp]
end

module Row' : sig
  include Delimited_intf.Row

  val create_of_fq : int String.Map.t -> string Fast_queue.t -> t
  val upgrade : ?header_map : int String.Map.t -> Delimited.Row.t -> t
  val header_map : t -> int String.Map.t
end = struct
  type t = { header_map : int String.Map.t
           ; fields : string array
           } [@@deriving compare, fields]

  let upgrade ?header_map delimited_row =
    let header_map =
      match header_map with
      | Some header_map -> header_map
      | None ->
        Delimited.Row.headers delimited_row
        |> Hashtbl.fold ~init:String.Map.empty ~f:(fun ~key ~data init ->
          Map.add init ~key ~data)
    in
    { header_map
    ; fields = Delimited.Row.to_array delimited_row
    }
  ;;

  let is_empty t = Array.for_all t.fields ~f:String.is_empty

  let sexp_of_t t =
    let names_by_indices =
      Map.to_sequence t.header_map
      |> Sequence.fold ~init:Int.Map.empty ~f:(fun init (name, index) ->
        Map.add init ~key:index ~data:name)
    in
    Array.mapi t.fields ~f:(fun i v ->
      let k =
        match Map.find names_by_indices i with
        | None -> Int.to_string i
        | Some name -> name
      in
      (k, v))
    |> [%sexp_of: (string * string) array]
  ;;

  let to_string t = Sexp.to_string_hum (sexp_of_t t)

  let index_exn t header =
    try String.Map.find_exn t.header_map header with
    | _ -> raise_s [%message "Header not found" (header : string)]
  ;;

  let get_exn_p t header here =
    let i = index_exn t header in
    try t.fields.(i) with
    | _ ->
      raise_s [%message "Header exists in file but not for this row"
                          (here : Source_code_position.t)
                          (header : string)
                          ~row:(t : t)]
  ;;

  let get_exn t header = get_exn_p t header [%here]

  let get_conv_exn t header here conv =
    let v = get_exn_p t header here in
    try conv v with
    | exn ->
      raise_s [%message "Failed to parse"
                          (here : Source_code_position.t)
                          (header : string)
                          ~row:(t : t)
                          (exn : exn)]
  ;;

  let get t header =
    try Some (get_exn t header) with
    | _ -> None
  ;;

  let get_opt_exn t header =
    match get t header with
    | None     -> raise_s [%message "No header in row" (header : string) ~row:(t : t)]
    | Some ""  -> None
    | Some str -> Some str
  ;;

  let get_conv_opt_exn t header here conv =
    match get_opt_exn t header with
    | None ->
      None
    | Some v ->
      try Some (conv v) with
      | exn ->
        raise_s [%message "Failed to parse"
                            (here : Source_code_position.t)
                            (header : string)
                            ~row:(t : t)
                            (exn : exn)]
  ;;

  let nth_exn t i = t.fields.(i)

  let nth_conv_exn t i here conv =
    try conv (nth_exn t i) with
    | exn ->
      raise_s [%message "Failed to parse"
                          (here : Source_code_position.t)
                          (i : int)
                          ~row:(t : t)
                          (exn : exn)]
  ;;

  let nth t i =
    try Some (nth_exn t i) with
    | _ -> None
  ;;

  let create header_table row_queue =
    { header_map =
        Hashtbl.fold header_table ~init:String.Map.empty ~f:(fun ~key ~data init ->
          Map.add init ~key ~data)
    ; fields = Queue.to_array row_queue
    }

  let create_of_fq header_map row_queue =
    { header_map
    ; fields = Fast_queue.to_array row_queue
    }

  let to_list t  = Array.to_list t.fields
  let to_array t = t.fields

  let equal t1 t2 = 0 = ([%compare: t] t1 t2)

  let fold t ~init ~f =
    Map.fold t.header_map ~init ~f:(fun ~key:header ~data:i acc ->
      f acc ~header ~data:(t.fields.(i)))
  ;;

  let iter t ~f = fold t ~init:() ~f:(fun () ~header ~data -> f ~header ~data)

  let size t = fold t ~init:0 ~f:(fun acc ~header:_ ~data -> acc + String.length data)

  let headers t = String.Table.of_alist_exn (Map.to_alist t.header_map)
end

let drop_lines ?(skip_lines=0) r =
  let rec loop n =
    if n = 0
    then Deferred.unit
    else begin
      Reader.read_line r
      >>= function
      | `Ok _ -> loop (n - 1)
      | `Eof  -> failwithf "file has fewer than %i lines" skip_lines ()
    end
  in
  loop skip_lines
;;

let strip_buffer buf =
  let len = Buffer.length buf in
  let rec first_non_space n =
    if n >= len
    then None
    else if Char.(<>) (Buffer.nth buf n) ' '
    then Some n
    else first_non_space (n + 1)
  in
  let rec last_non_space n =
    if n < 0
    then None
    else if Char.(<>) (Buffer.nth buf n) ' '
    then Some n
    else last_non_space (n - 1)
  in
  match first_non_space 0 with
  | None   -> ""
  | Some s ->
    match last_non_space (len - 1) with
    | None   -> assert false
    | Some e -> Buffer.sub buf s (e - s + 1)
;;

module Parse_state : sig
  type 'a t

  val acc : 'a t -> 'a

  val create
    :  ?strip : bool
    -> ?sep : char
    -> fields_used : int array option
    -> init : 'a
    -> f : (int -> 'a -> string Fast_queue.t -> 'a)
    -> unit
    -> 'a t

  val input : 'a t -> ?len : int -> string -> 'a t

  val finish : 'a t -> 'a t

end = struct

  module Step = struct
    type t =
      | Field_start
      | In_unquoted_field
      | In_quoted_field
      | In_quoted_field_after_quote
  end
  open Step

  type 'a t =
    { acc              : 'a
    ; sep              : char
    ; lineno           : int
    ; step             : Step.t
    ; field            : string
    ; current_row      : string list
    ; emit_field       : string Fast_queue.t -> Buffer.t -> unit
    ; f                : int -> 'a -> string Fast_queue.t -> 'a
    ; fields_used      : int array option
    ; current_field    : int
    ; next_field_index : int
    } [@@deriving fields]

  let make_emit_field ~strip current_row field =
    Fast_queue.enqueue
      current_row
      (if strip
       then strip_buffer field
       else Buffer.contents field);
    Buffer.clear field
  ;;

  let emit_row f i acc current_row =
    let acc = f i acc current_row in
    Fast_queue.clear current_row;
    acc
  ;;

  let create ?(strip=false) ?(sep=',') ~fields_used ~init ~f () =
    { acc = init
    ; sep
    ; lineno = 1
    ; step = Field_start
    ; field = ""
    ; current_row = []
    ; emit_field = make_emit_field ~strip
    ; f
    ; fields_used
    ; current_field = 0
    ; next_field_index = 0
    }
  ;;

  let mutable_of_t t =
    let field = Buffer.create (String.length t.field) in
    Buffer.add_string field t.field;
    let current_row = Fast_queue.of_list t.current_row in
    (field, current_row)
  ;;

  (* To reduce the number of allocations, we keep an array [fields_used] of the field
     indexes we care about. [current_field] is the position of the parser within the
     input row, and [next_field_index] is an index into the [fields_used] array
     indicating the next field that we need to store.

     If [fields_used] is None, we need to store every field.
  *)
  let should_enqueue fields_used current_field next_field_index =
    match fields_used with
    | None -> true
    | Some array -> next_field_index < Array.length array
                    && array.(next_field_index) = current_field

  let input t ?len input =
    let (field, current_row) = mutable_of_t t in
    let enqueue = ref (should_enqueue t.fields_used t.current_field t.next_field_index) in
    let current_field = ref t.current_field in
    let next_field_index = ref t.next_field_index in
    let increment_field () =
      current_field := !current_field + 1;
      next_field_index := if !enqueue then !next_field_index + 1 else !next_field_index;
      enqueue := should_enqueue t.fields_used !current_field !next_field_index
    in
    let reset_field () =
      current_field := 0;
      next_field_index := 0;
      enqueue := should_enqueue t.fields_used !current_field !next_field_index
    in
    let loop_bound = match len with
      | Some i -> i
      | None -> String.length input
    in
    let rec loop i t step =
      if i >= loop_bound
      then { t with step; current_field = !current_field; next_field_index = !next_field_index }
      else
        let open Char.Replace_polymorphic_compare in
        let continue = loop (i + 1) in
        let c = input.[i] in
        if c = '\r'
        then continue t step
        else
          match step with
          | Field_start ->
            if c = '\"'
            then continue t In_quoted_field
            else if c = t.sep
            then begin
              if !enqueue
              then t.emit_field current_row field;
              increment_field ();
              continue t Field_start
            end
            else if c = '\n'
            then begin
              if !enqueue
              then t.emit_field current_row field;
              reset_field ();
              continue { t with acc = emit_row t.f i t.acc current_row;
                                lineno = t.lineno + 1 }
                Field_start
            end
            else begin
              if !enqueue then Buffer.add_char field c;
              continue t In_unquoted_field
            end
          | In_unquoted_field ->
            begin
              if c = t.sep
              then begin
                if !enqueue
                then t.emit_field current_row field;
                increment_field ();
                continue t Field_start
              end
              else if c = '\n'
              then begin
                if !enqueue
                then t.emit_field current_row field;
                reset_field ();
                continue { t with acc = emit_row t.f i t.acc current_row;
                                  lineno = t.lineno + 1 }
                  Field_start
              end
              else begin
                if !enqueue then Buffer.add_char field c;
                continue t step
              end
            end
          | In_quoted_field ->
            if c = '\"'
            then continue t In_quoted_field_after_quote
            else begin
              if !enqueue then Buffer.add_char field c;
              continue t step
            end
          | In_quoted_field_after_quote ->
            if c = '\"'
            then begin (* doubled quote *)
              if !enqueue then Buffer.add_char field '"';
              continue t In_quoted_field
            end
            else if c = '0'
            then begin
              if !enqueue then Buffer.add_char field '\000';
              continue t In_quoted_field
            end
            else if c = t.sep
            then begin
              if !enqueue
              then t.emit_field current_row field;
              increment_field ();
              continue t Field_start
            end
            else if c = '\n'
            then begin
              if !enqueue
              then t.emit_field current_row field;
              reset_field ();
              continue { t with acc = emit_row t.f i t.acc current_row;
                                lineno = t.lineno + 1 }
                Field_start
            end
            else if Char.is_whitespace c
            then continue t step
            else
              failwithf "In_quoted_field_after_quote looking at '%c' (lineno=%d)"
                c (t.lineno) ()
    in
    let t' = loop 0 t t.step in
    { t' with
      field = Buffer.contents field
    ; current_row = Fast_queue.to_list current_row
    ; current_field = !current_field
    ; next_field_index = !next_field_index
    }
  ;;

  let finish t =
    let (field, current_row) = mutable_of_t t in
    let enqueue = should_enqueue t.fields_used t.current_field t.next_field_index in
    let acc = match t.step with
      | Field_start ->
        if Fast_queue.length current_row <> 0
        then begin
          if enqueue then t.emit_field current_row field;
          emit_row t.f 0 t.acc current_row
        end
        else t.acc
      | In_unquoted_field
      | In_quoted_field_after_quote ->
        if enqueue then t.emit_field current_row field;
        emit_row t.f 0 t.acc current_row
      | In_quoted_field ->
        raise (Delimited.Csv.Bad_csv_formatting
                 (Fast_queue.to_list current_row,
                  Buffer.contents field))
    in
    { t with
      field = Buffer.contents field
    ; current_row = Fast_queue.to_list current_row
    ; current_field = 0
    ; next_field_index = 0
    ; acc
    }
  ;;
end

module Builder = struct
  type _ t =
    | Column : int                                             -> string t
    | Header : string                                          -> string t
    | Return : 'a                                              -> 'a t
    | Apply  : ('b -> 'a) t * 'b t                             -> 'a t
    | Map    : ('b -> 'a) * 'b t                               -> 'a t
    | Map2   : ('b -> 'c -> 'a) * 'b t * 'c t                  -> 'a t
    | Both   : 'a t * 'b t                                     -> ('a * 'b) t
    | Lambda : (int String.Map.t -> string Fast_queue.t -> 'a) -> 'a t

  module T = struct
    let return x = Return x

    let apply f x =
      match f with
      | Return f -> Map (f, x)
      | Map (f, w) -> Map2 (f, w, x)
      | _ -> Apply (f, x)
    ;;

    let map x ~f =
      match x with
      | Return x -> Return (f x)
      | _ -> Map (f, x)
    ;;

    let map2 x y ~f =
      match (x, y) with
      | (Return x, Return y) -> Return (f x y)
      | _ -> Map2 (f, x, y)
    ;;

    let map3 x y z ~f =
      match (x, y, z) with
      | (Return x, Return y, Return z) -> Return (f x y z)
      | _ -> Apply (Map2 (f, x, y), z)
    ;;

    let all ts =
      List.fold_right ts ~init:(return []) ~f:(map2 ~f:(fun x xs -> x :: xs))
    ;;

    let both x y = Both (x, y)

    let ( <*> ) = apply
    let ( *> ) u v = return (fun () y -> y) <*> u <*> v
    let ( <* ) u v = return (fun x () -> x) <*> u <*> v

    module Applicative_infix = struct
      let ( <*> ) = ( <*> )
      let (  *> ) = (  *> )
      let ( <*  ) = ( <*  )
    end
  end
  include T

  let at_index i ~f = Map (f, Column i)
  let at_header h ~f = Map (f, Header h)
  let lambda f = Lambda f

  module Let_syntax = struct
    module Let_syntax = struct
      include T
      module Open_on_rhs = struct
        let at_index = at_index
        let at_header = at_header
      end
    end
  end

  module Without_headers = struct
    type 'a t =
      | Column : int -> string t
      | Return : 'a -> 'a t
      | Apply : ('b -> 'a) t * 'b t -> 'a t
      | Map : ('b -> 'a) * 'b t -> 'a t
      | Map2 : ('b -> 'c -> 'a) * 'b t * 'c t -> 'a t
      | Both : ('a t * 'b t) -> ('a * 'b) t
      | Lambda : (int String.Map.t -> string Fast_queue.t -> 'a) * int String.Map.t -> 'a t

    let get_fields_used t =
      let open Option.Let_syntax in
      let rec fields : type a. a t -> Int.Set.t option =
        function
        | Return _ -> Some Int.Set.empty
        | Column i -> Some (Int.Set.singleton i)
        | Apply (f, x) ->
          let%bind f = fields f in
          let%map x = fields x in
          Set.union f x
        | Map (_, x) -> fields x
        | Map2 (_, x, y) ->
          let%bind x = fields x in
          let%map y = fields y in
          Set.union x y
        | Both (x, y) ->
          let%bind x = fields x in
          let%map y = fields y in
          Set.union x y
        | Lambda _ -> None
      in
      fields t
    ;;

    let build t =
      let rec build' : type a. a t -> string Fast_queue.t -> a =
        fun t row ->
          match t with
          | Return x -> x
          | Column i -> Fast_queue.nth_exn row i
          | Apply (f, x) -> (build' f row) (build' x row)
          | Map (f, x) -> f (build' x row)
          | Map2 (f, x, y) -> f (build' x row) (build' y row)
          | Both (x, y) -> (build' x row, build' y row)
          | Lambda (f, header_map) -> f header_map row
      in
      let fields_used = get_fields_used t in
      match fields_used with
      | None -> (build' t, None)
      | Some fields_used ->
        let fields_used = Set.to_list fields_used in
        let mapping =
          List.mapi fields_used ~f:(fun i field_index -> (field_index, i))
          |> Int.Map.of_alist_exn
        in
        let rec remap : type a. a t -> a t = fun t ->
          match t with
          | Column i -> Column (Map.find_exn mapping i)
          | Return x -> Return x
          | Apply (f, x) -> Apply (remap f, remap x)
          | Map (f, x) -> Map (f, remap x)
          | Map2 (f, x, y) -> Map2 (f, remap x, remap y)
          | Both (x, y) -> Both (remap x, remap y)
          | Lambda _ -> t
        in
        (build' (remap t), Some (Array.of_list fields_used))
    ;;

  end

  let build ~header_map t =
    let rec transform : type a. a t -> a Without_headers.t = function
      | Return x -> Without_headers.Return x
      | Column i -> Without_headers.Column i
      | Header h -> Without_headers.Column (String.Map.find_exn header_map h)
      | Apply (f, x) -> Without_headers.Apply (transform f, transform x)
      | Map (f, x) -> Without_headers.Map (f, transform x)
      | Map2 (f, x, y) -> Without_headers.Map2 (f, transform x, transform y)
      | Both (x, y) -> Without_headers.Both (transform x, transform y)
      | Lambda f -> Without_headers.Lambda (f, header_map)
    in
    let transformed = transform t in
    Without_headers.build transformed

  let rec headers_used : type a. a t -> String.Set.t = function
    | Return _ -> String.Set.empty
    | Column _ -> String.Set.empty
    | Header h -> String.Set.singleton h
    | Apply (f, x) -> Set.union (headers_used f) (headers_used x)
    | Map (_, x) -> headers_used x
    | Map2 (_, x, y) -> Set.union (headers_used x) (headers_used y)
    | Both (x, y) -> Set.union (headers_used x) (headers_used y)
    | Lambda _ -> String.Set.empty
end

module Header_parse : sig
  (** Type [t] represents an incomplete header parse. Keep calling [input] on it until you
      get a map from header name to column number. *)
  type t

  val create
    :  ?strip : bool
    -> ?sep : char
    -> ?header : Header.t
    -> _ Builder.t
    -> (t, int String.Map.t) Either.t

  (** [input t ~len s] reads the first [len] bytes from [s] and returns either [t] or
      [header_map, unused_input]. *)
  val input : t -> len : int -> string -> (t, int String.Map.t * string) Either.t
  val of_reader : t -> Reader.t -> (int String.Map.t * string) Deferred.t
end = struct
  (* This exception is used to return early from the parser, so we don't consume more
     input than necessary. This is almost [With_return], except declaring the exception at
     top-level so we can freely pass around a closure that raises it. *)
  exception Header_parsed of string array * int

  type t =
    { state : unit Parse_state.t
    ; transform : string array -> int String.Map.t
    }

  let header_map header_row =
    Array.foldi header_row ~init:String.Map.empty ~f:(fun i map header ->
      Map.add map ~key:header ~data:i)
  ;;

  let limit_header builder limit_headers' csv_headers' =
    let limit_headers = String.Set.of_list limit_headers' in
    let builder_headers = Builder.headers_used builder in
    if not (Set.subset builder_headers limit_headers)
    then raise_s [%message "Builder uses header not specified in `Limit"
                             (builder_headers : String.Set.t)
                             (limit_headers : String.Set.t)];
    let csv_headers = String.Set.of_array csv_headers' in
    if not (Set.subset limit_headers csv_headers)
    then raise_s [%message "Header specified in `Limit not present in csv document"
                             (limit_headers : String.Set.t)
                             (csv_headers : String.Set.t)];
    header_map csv_headers'
  ;;

  let create' ?strip ?sep transform =
    let f offset () row = raise (Header_parsed (Fast_queue.to_array row, offset + 1)) in
    { state = Parse_state.create ?strip ?sep ~fields_used:None ~init:() ~f ()
    ; transform
    }
  ;;

  let create ?strip ?sep ?(header=`No) builder =
    match header with
    | `No              -> Second (String.Map.empty)
    | `Add headers     -> Second (header_map (Array.of_list headers))
    | `Yes             ->
      let f headers = limit_header builder (Array.to_list headers) headers in
      First  (create' ?strip ?sep f)
    | `Limit headers   ->
      let f csv_headers = limit_header builder headers csv_headers in
      First  (create' ?strip ?sep f)
    | `Replace headers ->
      let f _ = header_map (Array.of_list headers) in
      First  (create' ?strip ?sep f)
    | `Transform f     ->
      let f headers = header_map (Array.of_list (f (Array.to_list headers))) in
      First (create' ?strip ?sep f)
  ;;

  let input t ~len input =
    try First { t with state = Parse_state.input t.state ~len input; } with
    | Header_parsed (row, offset) ->
      Second (t.transform row, String.sub input ~pos:offset ~len:(len - offset))
  ;;

  let of_reader t r =
    let buffer = String.create buffer_size in
    Deferred.repeat_until_finished t (fun t ->
      match%map Reader.read r buffer ~len:buffer_size with
      | `Eof ->
        raise_s [%message "header is incomplete" ~_:([%here] : Source_code_position.t)]
      | `Ok len ->
        match input t ~len buffer with
        | First state -> `Repeat state
        | Second (headers, input) -> `Finished (headers, input))
  ;;
end

let fold_reader' ?strip ?skip_lines ?sep ?header ~builder ~init ~f r =
  let create_parser_state header_map builder =
    let (row_to_'a, fields_used) = Builder.build ~header_map builder in
    let init = Queue.create ~capacity:(Map.length header_map) () in
    let f _offset queue row =
      Queue.enqueue queue (row_to_'a row);
      queue
    in
    Parse_state.create ?strip ?sep ~fields_used ~init ~f ()
  in
  let%bind () = drop_lines r ?skip_lines in
  let%bind state =
    match Header_parse.create ?strip ?sep ?header builder with
    | Second header_map -> return (create_parser_state header_map builder)
    | First header_parse ->
      let%map (header_map, trailing_input) = Header_parse.of_reader header_parse r in
      Parse_state.input (create_parser_state header_map builder) trailing_input
  in
  let buffer = String.create buffer_size in
  Deferred.repeat_until_finished (state, init) (fun (state, init) ->
    match%bind Reader.read r buffer ~len:buffer_size with
    | `Eof ->
      let state = Parse_state.finish state in
      let%map init = f init (Parse_state.acc state) in
      `Finished init
    | `Ok i ->
      let state = Parse_state.input state buffer ~len:i in
      let%map init = f init (Parse_state.acc state) in
      Queue.clear (Parse_state.acc state);
      `Repeat (state, init))
;;

let bind_without_unnecessary_yielding x ~f =
  match Deferred.peek x with
  | Some x -> f x
  | None -> Deferred.bind x ~f
;;

let fold_reader ?strip ?skip_lines ?sep ?header ~builder ~init ~f r =
  fold_reader' ?strip ?skip_lines ?sep ?header ~builder ~init r ~f:(fun acc queue ->
    Queue.fold queue ~init:(return acc) ~f:(fun deferred_acc row ->
      bind_without_unnecessary_yielding deferred_acc ~f:(fun acc -> f acc row)))
;;

let fold_reader_without_pushback ?strip ?skip_lines ?sep ?header ~builder ~init ~f r =
  fold_reader' ?strip ?skip_lines ?sep ?header ~builder ~init r ~f:(fun acc queue ->
    return (Queue.fold queue ~init:acc ~f))
;;

let fold_reader_to_pipe ?strip ?skip_lines ?sep ?header ~builder reader =
  let r,w = Pipe.create () in
  let write_to_pipe : unit Deferred.t =
    fold_reader' ?strip ?skip_lines ?sep ?header ~builder ~init:() reader
      ~f:(fun () queue -> Pipe.transfer_in w ~from:queue)
    >>= fun () ->
    return (Pipe.close w)
  in
  don't_wait_for write_to_pipe;
  r
;;

let fold_string ?strip ?sep ?header ~builder ~init ~f csv_string =
  let header_map, csv_string =
    match Header_parse.create ?strip ?sep ?header builder with
    | Second header_map -> (header_map, csv_string)
    | First header_parse ->
      match Header_parse.input header_parse ~len:(String.length csv_string) csv_string with
      | First _ ->
        raise_s [%message "String ended mid-header row"
                            (csv_string : string)
                            (sep : char option)
                            (header : Header.t option)]
      | Second (header_map, csv_string) -> (header_map, csv_string)
  in
  let row_to_'a, fields_used = Builder.build ~header_map builder in
  let f init row = f init (row_to_'a row) in
  let state = Parse_state.create ?strip ?sep ~fields_used ~init ~f:(const f) () in
  Parse_state.input state csv_string
  |> Parse_state.finish
  |> Parse_state.acc
;;

module Replace_delimited_csv = struct

  module Delimited_deprecated = Delimited

  module Delimited = struct

    module Header = Header
    module Row = struct
      include Row'
      let builder = Builder.lambda create_of_fq
    end

    module Csv = struct
      exception Bad_csv_formatting = Delimited.Csv.Bad_csv_formatting

      let manual_parse_data parse_state input =
        let parse_state =
          match input with
          | `Eof -> Parse_state.finish parse_state
          | `Data s -> Parse_state.input parse_state s
        in
        let queue = Parse_state.acc parse_state in
        let result = Fast_queue.to_list queue in
        Fast_queue.clear queue;
        Second parse_state, result
      ;;

      let create_parse_state ?strip ?sep header_map =
        Parse_state.create ?strip ?sep
          ~fields_used:None
          ~init:(Fast_queue.create ())
          ~f:(fun _ queue row ->
            Fast_queue.enqueue queue (Row.create_of_fq header_map row);
            queue)
          ()
      ;;

      let manual_parse_header ?strip ?sep header_state input =
        let input =
          match input with
          | `Eof -> ""
          | `Data s -> s
        in
        match Header_parse.input header_state ~len:(String.length input) input with
        | First header_state -> First header_state, []
        | Second (header_map, input) ->
          let state = create_parse_state ?strip ?sep header_map in
          manual_parse_data state (`Data input)
      ;;

      let create_manual ?strip ?sep ~header () =
        let state =
          Header_parse.create ?strip ?sep ~header (Builder.return ())
          |> Either.Second.map ~f:(create_parse_state ?strip ?sep)
          |> ref
        in
        let parse_chunk input =
          let state', results =
            match !state with
            | First  state -> manual_parse_header ?strip ?sep state input
            | Second state -> manual_parse_data               state input
          in
          state := state';
          results
        in
        stage parse_chunk
      ;;

      let of_reader ?strip ?skip_lines ?sep ~header reader =
        fold_reader_to_pipe ?strip ?skip_lines ?sep ~header
          ~builder:Row.builder reader
      ;;

      let create_reader ?strip ?skip_lines ?sep ~header filename =
        Reader.open_file filename
        >>| of_reader ?strip ?skip_lines ?sep ~header
      ;;

      let parse_string ?strip ?sep ~header csv_string =
        fold_string ?strip ?sep ~header csv_string
          ~builder:Row.builder
          ~init:(Fast_queue.create ())
          ~f:(fun queue row -> Fast_queue.enqueue queue row; queue)
        |> Fast_queue.to_list
      ;;

      let of_writer = Delimited.Csv.of_writer
      let create_writer = Delimited.Csv.create_writer

    end

    type ('a,'b) reader = ('a, 'b) Delimited.reader

    let upgrade_delimited_row_pipe r =
      Pipe.fold_map r ~init:None ~f:(fun header_map row ->
        let row = Row.upgrade ?header_map row in
        (Some (Row.header_map row), row))
    ;;

    module Positional = struct
      include Delimited.Positional

      let of_reader ?strip ?skip_lines ?on_parse_error ~header ?strict r =
        Delimited.Positional.of_reader ?strip ?skip_lines ?on_parse_error ~header ?strict r
        |> Or_error.map ~f:upgrade_delimited_row_pipe
      ;;

      let create_reader ?strip ?skip_lines ?on_parse_error ~header ?strict filename =
        Delimited.Positional.create_reader ?strip ?skip_lines ?on_parse_error ~header
          ?strict filename
        >>| Or_error.map ~f:upgrade_delimited_row_pipe
      ;;
    end

    let of_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep r =
      Delimited.of_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep r
      |> upgrade_delimited_row_pipe
    ;;

    let create_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep filename =
      Delimited.create_reader ?strip ?skip_lines ?on_parse_error ~header ?quote ~sep
        filename
      >>| upgrade_delimited_row_pipe
    ;;
  end
end

let%bench_module "Fast_queue" =
  (module struct
    (* 2016-07-22: Our [enqueue] without resizing ("enqueue+clear" - "clear") takes about
       1/3 as much time as [Core_queue], and other operations are comparable or better.

        | Name                     | Time/Run | mWd/Run |
        |--------------------------+----------+---------|
        | Fast_queue.enqueue+clear | 4.69ns   |         |
        | Core_queue.enqueue+clear | 8.56ns   |         |
        | Fast_queue.clear         | 2.54ns   |         |
        | Core_queue.clear         | 2.76ns   |         |
        | Fast_queue.nth_exn       | 3.44ns   |         |
        | Core_queue.nth_exn       | 4.77ns   |         |
        | Fast_queue.to_array:100  | 611.61ns | 105.00w |
        | Core_queue.to_array:100  | 611.79ns | 105.00w |

       Weirdly, if [enqueue] needs to resize, it takes twice as long as [Core_queue],
       despite doing strictly less work, and that work by an almost-verbatim copy of the
       [Core_queue] code. Flamegraphs look the same too.

        | Name               | Time/Run | mWd/Run | mjWd/Run |
        |--------------------+----------+---------+----------|
        | Fast_queue.enqueue | 161.90ns |         | 5.58w    |
        | Core_queue.enqueue | 99.81ns  |         | 5.55w    |

       Fortunately, our workload only requires a few resizes in the first row, and usually
       no more for the rest of the file. *)
    module M : module type of struct include Fast_queue end = struct
      type 'a t = 'a Fast_queue.t

      let create = Fast_queue.create

      let%bench_fun "Fast_queue.create" [@indexed n = [ 10; 100; 1000; ] ] =
fun () -> Fast_queue.create ~capacity:n ()

let%bench_fun "Core_queue.create" [@indexed n = [ 10; 100; 1000; ] ] =
fun () -> Queue.create ~capacity:n ()

let of_list = Fast_queue.of_list

let%bench_fun "Fast_queue.of_list" [@indexed n = [ 10; 100; 1000; ] ] =
let l = List.init n ~f:Fn.id in
fun () -> Fast_queue.of_list l

let%bench_fun "Core_queue.of_list" [@indexed n = [ 10; 100; 1000; ] ] =
let l = List.init n ~f:Fn.id in
fun () -> Queue.of_list l

let enqueue = Fast_queue.enqueue

let%bench_fun "Fast_queue.enqueue" =
  let t = Fast_queue.create () in
  fun () -> Fast_queue.enqueue t ()

let%bench_fun "Core_queue.enqueue" =
  let t = Queue.create () in
  fun () -> Queue.enqueue t ()

let%bench_fun "Fast_queue.enqueue+clear" =
  let t = Fast_queue.create () in
  fun () ->
    Fast_queue.enqueue t ();
    Fast_queue.clear t

let%bench_fun "Core_queue.enqueue+clear" =
  let t = Queue.create () in
  fun () ->
    Queue.enqueue t ();
    Queue.clear t

let nth_exn = Fast_queue.nth_exn

let%bench_fun "Fast_queue.nth_exn" =
  let t = Fast_queue.of_list [ () ] in
  fun () -> Fast_queue.nth_exn t 0

let%bench_fun "Core_queue.nth_exn" =
  let t = Queue.of_list [ () ] in
  fun () -> Queue.get t 0

let clear = Fast_queue.clear

let%bench_fun "Fast_queue.clear" =
  let t = Fast_queue.create () in
  fun () -> Fast_queue.clear t

let%bench_fun "Core_queue.clear" =
  let t = Queue.create () in
  fun () -> Queue.clear t

let to_list = Fast_queue.to_list

let%bench_fun "Fast_queue.to_list" [@indexed n = [ 10; 100; 1000; ] ] =
let t = Fast_queue.of_list (List.init n ~f:Fn.id) in
fun () -> Fast_queue.to_list t

let%bench_fun "Core_queue.to_list" [@indexed n = [ 10; 100; 1000; ] ] =
let t = Queue.of_list (List.init n ~f:Fn.id) in
fun () -> Queue.to_list t

let to_array = Fast_queue.to_array

let%bench_fun "Fast_queue.to_array" [@indexed n = [ 10; 100; 1000; ] ] =
let t = Fast_queue.of_list (List.init n ~f:Fn.id) in
fun () -> Fast_queue.to_array t

let%bench_fun "Core_queue.to_array" [@indexed n = [ 10; 100; 1000; ] ] =
let t = Queue.of_list (List.init n ~f:Fn.id) in
fun () -> Queue.to_array t

let length = Fast_queue.length

let%bench_fun "Fast_queue.length" =
  let t = Fast_queue.create () in
  fun () -> Fast_queue.length t

let%bench_fun "Core_queue.length" =
  let t = Queue.create () in
  fun () -> Queue.length t


end
end)
