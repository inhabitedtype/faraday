open Faraday


val serialize
  :  t
  -> yield  : (t -> unit Lwt.t)
  -> writev : (buffer iovec list -> [ `Ok of int | `Closed ] Lwt.t)
  -> unit Lwt.t
