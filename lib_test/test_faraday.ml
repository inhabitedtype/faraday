open Faraday

let bigstring_of_string str =
  let len = String.length str in
  let buf = Bigarray.(Array1.create char c_layout len) in
  for i = 0 to len - 1 do
    Bigarray.Array1.unsafe_set buf i (String.unsafe_get str i)
  done;
  buf

let rec cross xs ys =
  match xs with
  | [] -> []
  | x::xs' -> List.(map (fun y -> [x; y]) ys) @ (cross xs' ys)

let check ?(buf_size=0x100) ~iovecs ~msg ops result =
  let t = create buf_size in
  List.iter (function
    | `Write_string    s -> write_string    t s
    | `Write_bytes     s -> write_bytes     t (Bytes.unsafe_of_string s)
    | `Write_bigstring s -> write_bigstring t (bigstring_of_string s)
    | `Write_char      c -> write_char      t c
    | `Schedule_string s -> schedule_string t s
    | `Schedule_bytes  s -> schedule_bytes  t (Bytes.unsafe_of_string s)
    | `Schedule_bigstring s -> schedule_bigstring t (bigstring_of_string s)
    | `Yield -> yield t)
  ops;
  Alcotest.(check int) "iovec count" iovecs
    (match operation t with
    | `Writev iovecs  -> List.length iovecs
    | _               -> 0);
  Alcotest.(check string) msg result (serialize_to_string t)

let empty =
  [ "noop"       , `Quick, begin fun () -> check ~iovecs:0 ~msg:"noop"  []       "" end
  ; "yield"      , `Quick, begin fun () -> check ~iovecs:0 ~msg:"yield" [`Yield] "" end
  ; "write", `Quick, begin fun () ->
      check ~iovecs:0 ~msg:"string"    [`Write_string    ""] "";
      check ~iovecs:0 ~msg:"bytes"     [`Write_bytes     ""] "";
      check ~iovecs:0 ~msg:"bigstring" [`Write_bigstring ""] ""
  end
  ; "schedule", `Quick, begin fun () ->
      check ~iovecs:0 ~msg:"string"    [`Schedule_string    ""] "";
      check ~iovecs:0 ~msg:"bytes"     [`Schedule_bytes     ""] "";
      check ~iovecs:0 ~msg:"bigstring" [`Schedule_bigstring ""] ""
  end ]

let write =
  [ "single", `Quick, begin fun () ->
      check ~iovecs:1 ~msg:"string"    [`Write_string    "test"] "test";
      check ~iovecs:1 ~msg:"bytes"     [`Write_bytes     "test"] "test";
      check ~iovecs:1 ~msg:"bigstring" [`Write_bigstring "test"] "test";
      check ~iovecs:1 ~msg:"char"      [`Write_char      'A'   ] "A";
      check ~buf_size:1 ~iovecs:4 ~msg:"string"    [`Write_string    "test"] "test";
      check ~buf_size:1 ~iovecs:4 ~msg:"bytes"     [`Write_bytes     "test"] "test";
      check ~buf_size:1 ~iovecs:4 ~msg:"bigstring" [`Write_bigstring "test"] "test";
  end ]

let write_tiny_buf =
  [ "single with tiny buffer", `Quick, begin fun () ->
      check ~buf_size:1 ~iovecs:4 ~msg:"string"    [`Write_string    "test"] "test";
      check ~buf_size:1 ~iovecs:4 ~msg:"bytes"     [`Write_bytes     "test"] "test";
      check ~buf_size:1 ~iovecs:4 ~msg:"bigstring" [`Write_bigstring "test"] "test";
  end ]

let schedule =
  [ "single", `Quick, begin fun () ->
      check ~iovecs:1 ~msg:"string"    [`Schedule_string    "test"] "test";
      check ~iovecs:1 ~msg:"bytes"     [`Schedule_bytes     "test"] "test";
      check ~iovecs:1 ~msg:"bigstring" [`Schedule_bigstring "test"] "test"
  end ]

let interleaved =
  (* XXX(seliopou): Replace with property-based testing. The property should
     really be: Given a string, for any partition of that string and for any
     assignment of writes and schedules on the partition, the output will be
     the same as the input. *)
  [ "write_then_schedule", `Quick, begin fun () ->
    List.iteri (fun i ops ->
      check ~iovecs:2 ~msg:(Printf.sprintf "write_then_schedule: %d" i) ops "test")
    (cross
      [`Write_string "te"; `Write_bytes "te"; `Write_bigstring "te"]
      [`Schedule_string "st"; `Schedule_bytes "st"; `Schedule_bigstring "st"]);
    List.iteri (fun i ops ->
      check ~iovecs:2 ~msg:"write_then_schedule: char" ops "test")
    (cross
      [`Write_char 't']
      [`Schedule_string "est"; `Schedule_bytes "est"; `Schedule_bigstring "est"])
  end
  ; "schedule_then_write", `Quick, begin fun () ->
    List.iteri (fun i ops ->
      check ~iovecs:2 ~msg:(Printf.sprintf "schedule_then_write: %d" i) ops "stte")
    (cross
      [`Schedule_string "st"; `Schedule_bytes "st"; `Schedule_bigstring "st"]
      [`Write_string "te"; `Write_bytes "te"; `Write_bigstring "te"]);
    List.iteri (fun i ops ->
      check ~iovecs:2 ~msg:"schedule_then_write: char" ops "estt")
    (cross
      [`Schedule_string "est"; `Schedule_bytes "est"; `Schedule_bigstring "est"]
      [`Write_char 't'])
  end ]

let () =
  Alcotest.run "test suite"
    [ "empty output"              , empty
    ; "single write"              , write
    ; "single write (tiny buffer)", write
    ; "single schedule"           , schedule
    ; "interleaved calls"         , interleaved ]
