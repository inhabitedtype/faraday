include Faraday_lwt

open Lwt.Infix

let writev_of_fd fd =
  fun iovecs ->
    let lwt_iovecs = Lwt_unix.IO_vectors.create () in
    iovecs |> List.iter (fun {Faraday.buffer; off; len} ->
      Lwt_unix.IO_vectors.append_bigarray lwt_iovecs buffer off len);

    Lwt.catch
      (fun () ->
        Lwt_unix.writev fd lwt_iovecs
        >|= fun n -> `Ok n)
      (function
      | Unix.Unix_error (Unix.EBADF, "check_descriptor", _)
      | Unix.Unix_error (Unix.EPIPE, _, _) ->
        Lwt.return `Closed
      | exn ->
        Lwt.fail exn)
