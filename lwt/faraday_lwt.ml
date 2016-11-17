open Lwt

let serialize t ~yield ~writev =
  let shutdown () =
    Faraday.close t;
    (* It's necessary to drain the serializer in order to free any buffers that
     * may be be queued up. *)
    ignore (Faraday.drain t);
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
  catch
    (fun ()  -> loop t)
    (fun exn ->
      shutdown ();
      fail exn)
