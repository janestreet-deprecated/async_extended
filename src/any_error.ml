open Core.Std
open Async.Std

type ('ok, 'err) t = ('ok, 'err) Deferred.Result.t

let return = Deferred.Result.return
let map = Deferred.Result.map

let both at bt =
  Deferred.choose
    [ Deferred.choice at (fun x -> `A x)
    ; Deferred.choice bt (fun x -> `B x)
    ]
  >>= function
  | `A (Error _ as err)
  | `B (Error _ as err) -> Deferred.return err
  | `A (Ok a) -> map bt ~f:(fun b -> (a, b))
  | `B (Ok b) -> map at ~f:(fun a -> (a, b))

let (<*>) = fun abt at -> map (both abt at) ~f:(fun (f, x) -> f x)

let map2 at bt ~f = map (both at bt) ~f:(fun (a,b) -> f a b)

let all ts =
  let cons x xs = x :: xs in
  let rec go acc = function
    | [] -> map acc ~f:List.rev
    | t :: ts -> go (return cons <*> t <*> acc) ts
  in
  go (return []) ts

let all_ignore ts = Deferred.create (fun r ->
  Deferred.List.iter ~how:`Parallel ts ~f:(fun t ->
    t >>| function
    | Error _ as error -> Ivar.fill_if_empty r error
    | Ok () -> ())
  >>> fun () ->
  Ivar.fill_if_empty r (Ok ()));
