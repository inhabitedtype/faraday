open Faraday


val serialize
  :  t
  -> yield  : (t -> unit Lwt.t)
  -> writev : (bigstring iovec list -> [ `Ok of int | `Closed ] Lwt.t)
  -> unit Lwt.t
