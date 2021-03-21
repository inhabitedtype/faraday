open Core
open Async

module Unix = Core.Unix


let serialize t ~yield ~writev =
  let shutdown () =
    Faraday.close t;
    (* It's necessary to drain the serializer in order to free any buffers that
     * be queued up. *)
    ignore (Faraday.drain t)
  in
  let rec loop t =
    match Faraday.operation t with
    | `Writev iovecs ->
      writev iovecs
      >>= (function
        | `Closed   -> shutdown (); return () (* XXX(seliopou): this should be reported *)
        | `Ok n     -> Faraday.shift t n; loop t)
    | `Yield ->
      yield t >>= fun () -> loop t
    | `Close -> return ()
  in
  try_with
    ~rest:`Log (* consider [`Raise] instead *)
    ~run:`Schedule (* consider [`Now] instead *)
    ~extract_exn:true (fun () -> loop t)
  >>| function
    | Result.Ok () -> ()
    | Result.Error exn ->
      shutdown ();
      raise exn

let writev_of_fd fd =
  let badfd =
    failwithf "writev_of_fd got bad fd: %s" (Fd.to_string fd)
  in
  let finish result =
    let open Unix.Error in
    match result with
    | `Ok n           -> return (`Ok n)
    | `Already_closed -> return `Closed
    | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
      begin Fd.ready_to fd `Write
      >>| function
        | `Bad_fd -> badfd ()
        | `Closed -> `Closed
        | `Ready  -> `Ok 0
      end
    | `Error (Unix.Unix_error (EBADF, _, _)) ->
      badfd ()
    | `Error exn ->
      Deferred.don't_wait_for (Fd.close fd);
      raise exn
  in
  fun iovecs ->
    let iovecs = Array.of_list_map iovecs ~f:(fun iovec ->
      let { Faraday.buffer; off = pos; len } = iovec in
      Unix.IOVec.of_bigstring ~pos ~len buffer)
    in
    if Fd.supports_nonblock fd then
      finish
        (Fd.syscall fd ~nonblocking:true
           (fun file_descr ->
             Bigstring_unix.writev_assume_fd_is_nonblocking file_descr iovecs))
    else
      Fd.syscall_in_thread fd ~name:"writev"
        (fun file_descr -> Bigstring_unix.writev file_descr iovecs)
      >>= finish
