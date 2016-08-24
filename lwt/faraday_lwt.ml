open Lwt

let serialize t ~yield ~writev =
  let shutdown () =
    Faraday.close t;
    (* It's necessary to drain the serializer in order to free any buffers that
     * may be be queued up. *)
    Faraday.drain t;
  in
  let rec loop op =
    match op with
    | Faraday.Writev(iovecs, k) ->
      writev iovecs
      >>= (function
        | `Closed   -> shutdown (); return () (* XXX(seliopou): this should be reported *)
        | `Ok n     -> loop (k n))
    | Faraday.Yield k ->
      yield t >>= fun () -> loop (k ())
    | Faraday.Close -> return ()
  in
  catch
    (fun ()  -> loop (Faraday.serialize t))
    (fun exn ->
      shutdown ();
      fail exn)
