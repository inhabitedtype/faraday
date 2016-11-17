type file_descr = Unix.file_descr
let bytes_unsafe_to_string = Bytes.unsafe_to_string

open Core.Std
open Async.Std

module Unix = Core.Std.Unix


let serialize t ~yield ~writev =
  let shutdown () =
    Faraday.close t;
    (* It's necessary to drain the serializer in order to free any buffers that
     * be queued up. *)
    Faraday.drain t
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
  try_with ~extract_exn:true (fun () -> loop t)
  >>| function
    | Result.Ok () -> ()
    | Result.Error exn ->
      shutdown ();
      raise exn

let take_group iovecs =
  let to_group = function
    | { Faraday.buffer = `String buffer; off; len } ->
      `String [{ Faraday.buffer; off; len }]
    | { Faraday.buffer = `Bytes  buffer; off; len } ->
      `String [{ Faraday.buffer = bytes_unsafe_to_string buffer; off; len }]
    | { Faraday.buffer = `Bigstring buffer; off; len } ->
      `Bigstring [{ Faraday.buffer; off; len }]
  in
  let rec loop group iovecs =
    match iovecs with
    | [] -> (group, [])
    | iovec::iovecs ->
      begin match to_group iovec, group with
      | `String item   , `String group ->
        loop (`String (item @ group)) iovecs
      | `Bigstring item, `Bigstring group ->
        loop (`Bigstring (item @ group)) iovecs
      | item, _ -> group, (iovec::iovecs)
      end
  in
  match iovecs with
  | [] -> None
  | iovec::iovecs ->
    let group, rest = loop (to_group iovec) iovecs in
    Some(group, rest)

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
    match take_group iovecs with
    | None -> return (`Ok 0)
    | Some(`String group, _) ->
      let iovecs = Array.of_list_rev_map group ~f:(fun iovec ->
        let { Faraday.buffer; off = pos; len } = iovec in
        Unix.IOVec.of_string ~pos ~len buffer)
      in
      if Fd.supports_nonblock fd then
        finish
          (Fd.syscall fd ~nonblocking:true
            (fun file_descr ->
              Unix.writev_assume_fd_is_nonblocking file_descr iovecs))
      else
        Fd.syscall_in_thread fd ~name:"writev"
          (fun file_descr -> Unix.writev file_descr iovecs)
        >>= finish
    | Some(`Bigstring group, _) ->
      let iovecs = Array.of_list_rev_map group ~f:(fun iovec ->
        let { Faraday.buffer; off = pos; len } = iovec in
        Unix.IOVec.of_bigstring ~pos ~len buffer)
      in
      if Fd.supports_nonblock fd then
        finish
          (Fd.syscall fd ~nonblocking:true
            (fun file_descr ->
              Bigstring.writev_assume_fd_is_nonblocking file_descr iovecs))
      else
        Fd.syscall_in_thread fd ~name:"writev"
          (fun file_descr -> Bigstring.writev file_descr iovecs)
        >>= finish
