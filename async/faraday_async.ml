type file_descr = Unix.file_descr
let bytes_unsafe_to_string = Bytes.unsafe_to_string

open Core.Std
open Async.Std

module Unix = Core.Std.Unix


let serialize t ~yield ~writev =
  let rec loop op =
    match op with
    | Faraday.Writev(iovecs, k) ->
      writev iovecs
      >>= (function
        | `Closed n -> Faraday.close t; loop (k n)
        | `Ok n     -> loop (k n))
    | Faraday.Yield k ->
      yield t >>= fun () -> loop (k ())
    | Faraday.Close -> return ()
  in
  loop (Faraday.serialize t)

let group_iovecs iovecs =
  let to_group = function
    | { Faraday.buffer = `String buffer; off; len } ->
      `String [{ Faraday.buffer; off; len }]
    | { Faraday.buffer = `Bytes  buffer; off; len } ->
      `String [{ Faraday.buffer = bytes_unsafe_to_string buffer; off; len }]
    | { Faraday.buffer = `Bigstring buffer; off; len } ->
      `Bigstring [{ Faraday.buffer; off; len }]
  in
  let rec loop iovecs group acc =
    match iovecs with
    | [] -> List.rev (group::acc)
    | iovec::iovecs ->
      begin match to_group iovec, group with
      | `String item   , `String group ->
        loop iovecs (`String (item @ group)) acc
      | `Bigstring item, `Bigstring group ->
        loop iovecs (`Bigstring (item @ group)) acc
      | item, _ ->
        loop iovecs item (group::acc)
      end
  in
  match iovecs with
  | []            -> assert false
  | iovec::iovecs -> loop iovecs (to_group iovec) []

let serialize_to_fd t fd ~yield =
  let writev iovecs =
    let rec loop groups written =
      match groups with
      | []                      -> return (`Ok written)
      | (`String group)::groups ->
        let iovecs = Array.of_list_rev_map group ~f:(fun iovec ->
          let { Faraday.buffer; off = pos; len } = iovec in
          Unix.IOVec.of_string ~pos ~len buffer)
        in
        finish groups written
          (Fd.syscall fd ~nonblocking:true
            (fun file_descr ->
              Unix.writev_assume_fd_is_nonblocking file_descr iovecs))
      | (`Bigstring group)::groups ->
        let iovecs = Array.of_list_rev_map group ~f:(fun iovec ->
          let { Faraday.buffer; off = pos; len } = iovec in
          Unix.IOVec.of_bigstring ~pos ~len buffer)
        in
        finish groups written
          (Fd.syscall fd ~nonblocking:true
            (fun file_descr ->
              Bigstring.writev_assume_fd_is_nonblocking file_descr iovecs))
    and finish groups written result =
      let open Unix.Error in
      match result with
      | `Ok n           -> loop groups (written + n)
      | `Already_closed -> return (`Closed written)
      | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
        begin Fd.ready_to fd `Write
        >>| function
          | `Bad_fd -> Faraday.close t; failwith "serialize_to_fd got bad fd"
          | `Closed -> `Closed written
          | `Ready  -> `Ok written
        end
      | `Error (Unix.Unix_error (EBADF, _, _)) ->
        Faraday.close t; failwith "serialize_to_fd got bad fd"
      | `Error exn ->
        Faraday.close t;
        Deferred.don't_wait_for (Fd.close fd);
        raise exn
    in
    loop (group_iovecs iovecs) 0
  in
  serialize t ~yield ~writev
