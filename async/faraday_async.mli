open! Core
open Async

open Faraday


val serialize
  :  Faraday.t
  -> yield  : (t -> unit Deferred.t)
  -> writev : (bigstring iovec list -> [ `Ok of int | `Closed ] Deferred.t)
  -> unit Deferred.t

val writev_of_fd
  :  Fd.t
  -> bigstring iovec list -> [ `Ok of int | `Closed ] Deferred.t
