open Core.Std;;
open Async.Std;;
module Find = Async_extended.Std.Find;;
module Stats = Unix.Stats
module FO = Find.Options


let file_list ~inc ~exc path =
  let filter = Some (fun (fn,stat) ->
      let file = stat.Stats.kind = `File in
      let included = Pcre.pmatch ~pat:inc fn in
      let excluded = match exc with None -> false | Some pat -> Pcre.pmatch ~pat fn in
      return (file && (included && not excluded))
    )
  in
  let options = {FO.default with FO.filter = filter} in
  Find.to_list (Find.create ~options path)
;;

let _ =
  let path, pattern = (Varg.parse2 ()) in
  upon (file_list ~inc:pattern ~exc:None path) (fun file_list ->
    List.iter file_list ~f:(fun (file, _) -> Print.printf "%s\n%!" file);
    shutdown 0;
  );
  never_returns (Scheduler.go ())
;;
