open Core.Std
open Async.Std

val input_sexps : Reader.t -> Sexp.t list Deferred.t

val open_gzip_file : string -> Reader.t Deferred.t

val with_gzip_file        : string             -> f:(Reader.t -> 'a Deferred.t) -> 'a Deferred.t
val with_hadoop_gzip_file : hadoop_file:string ->   (Reader.t -> 'a Deferred.t) -> 'a Deferred.t
val with_xzip_file        : string             -> f:(Reader.t -> 'a Deferred.t) -> 'a Deferred.t
