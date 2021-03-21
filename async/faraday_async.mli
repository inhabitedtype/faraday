open! Core
open Async

open Faraday


val serialize
  :  Faraday.t
  -> yield  : (t -> unit Deferred.t)
  -> writev : (IOVec.t list -> [ `Ok of int | `Closed ] Deferred.t)
  -> unit Deferred.t

val writev_of_fd
  :  Fd.t
  -> IOVec.t list -> [ `Ok of int | `Closed ] Deferred.t
