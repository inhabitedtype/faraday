open Faraday


include module type of Faraday_lwt

val writev_of_fd
  :  Lwt_unix.file_descr
  -> buffer iovec list -> [ `Ok of int | `Closed ] Lwt.t
