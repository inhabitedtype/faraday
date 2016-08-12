open Core.Std
open Async.Std

open Faraday


val serialize_to_fd
  :  t
  -> Fd.t
  -> yield : (t -> unit Deferred.t)
  -> unit Deferred.t

val serialize
  :  Faraday.t
  -> yield  : (t -> unit Deferred.t)
  -> writev : (buffer iovec list -> [`Ok of int | `Closed of int] Deferred.t)
  -> unit Deferred.t
