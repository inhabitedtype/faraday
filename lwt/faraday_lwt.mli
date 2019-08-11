open Faraday


val serialize
  :  t
  -> yield  : (t -> unit Lwt.t)
  -> writev : (IOVec.t list -> [ `Ok of int | `Closed ] Lwt.t)
  -> unit Lwt.t
