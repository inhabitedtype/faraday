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
    | `Write_le        i -> LE.write_uint16 t i
    | `Write_be        i -> BE.write_uint16 t i
    | `Write_string    s -> write_string    t s
    | `Write_bytes     s -> write_bytes     t (Bytes.unsafe_of_string s)
    | `Write_bigstring s -> write_bigstring t (bigstring_of_string s)
    | `Write_char      c -> write_char      t c
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
      check ~iovecs:0 ~msg:"bigstring" [`Schedule_bigstring ""] ""
  end ]

let endian =
  [ "endian", `Quick, begin fun () ->
      check ~iovecs:1 ~msg:"uint16 le" [`Write_le 5] "\005\000";
      check ~iovecs:1 ~msg:"uint16 be" [`Write_be 5] "\000\005"
  end ]

let write =
  [ "single", `Quick, begin fun () ->
      check ~iovecs:1 ~msg:"string"    [`Write_string    "test"] "test";
      check ~iovecs:1 ~msg:"bytes"     [`Write_bytes     "test"] "test";
      check ~iovecs:1 ~msg:"bigstring" [`Write_bigstring "test"] "test";
      check ~iovecs:1 ~msg:"char"      [`Write_char      'A'   ] "A"
  end ]

let write_tiny_buf =
  [ "single with tiny buffer", `Quick, begin fun () ->
      check ~buf_size:1 ~iovecs:1 ~msg:"string"    [`Write_string    "test"] "test";
      check ~buf_size:1 ~iovecs:1 ~msg:"bytes"     [`Write_bytes     "test"] "test";
      check ~buf_size:1 ~iovecs:1 ~msg:"bigstring" [`Write_bigstring "test"] "test"
  end 
  ; "multiple writes with tiny buffer", `Quick,  begin fun () ->
      check ~buf_size:1 ~iovecs:2 ~msg:"string" [`Write_string "test1"; `Write_string "test2"] "test1test2"
  end
  ; "too many writes with tiny buffer", `Quick,  begin fun () ->
      check ~buf_size:1 ~iovecs:5 ~msg:"string"
        [`Write_string "te"; `Write_string "st"; `Write_string "te"; `Write_string "st"; `Write_string "te" ] 
        "testtestte"
  end ]

let schedule =
  [ "single", `Quick, begin fun () ->
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
      [`Schedule_bigstring "st"]);
    List.iteri (fun i ops ->
      check ~iovecs:2 ~msg:"write_then_schedule: char" ops "test")
    (cross
      [`Write_char 't'; `Write_string "t"; `Write_bytes "t"]
      [`Schedule_bigstring "est"])
  end
  ; "schedule_then_write", `Quick, begin fun () ->
    List.iteri (fun i ops ->
      check ~iovecs:2 ~msg:(Printf.sprintf "schedule_then_write: %d" i) ops "stte")
    (cross
      [`Schedule_bigstring "st"]
      [`Write_string "te"; `Write_bytes "te"; `Write_bigstring "te"]);
    List.iteri (fun i ops ->
      check ~iovecs:2 ~msg:"schedule_then_write: char" ops "estt")
    (cross
      [`Schedule_bigstring "est"]
      [`Write_char 't'; `Write_bytes "t"; `Write_string "t"])
  end ]

let () =
  Alcotest.run "test suite"
    [ "empty output"              , empty
    ; "endianness"                , endian
    ; "single write"              , write
    ; "writes (tiny buffer)"      , write_tiny_buf
    ; "single schedule"           , schedule
    ; "interleaved calls"         , interleaved ]
