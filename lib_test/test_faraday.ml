open Faraday

let bigstring_of_string str =
  let len = String.length str in
  let buf = Bigarray.(Array1.create char c_layout len) in
  for i = 0 to len - 1 do
    Bigarray.Array1.unsafe_set buf i (String.unsafe_get str i)
  done;
  buf

let check ?(buf_size=0x100) ~msg f result =
  let t = create buf_size in
  f t;
  Alcotest.(check string) msg result (serialize_to_string t)

let empty =
  let empty_bytes = Bytes.create 0 in
  let empty_bigstring = bigstring_of_string "" in
  [ "noop"       , `Quick, begin fun () -> check ~msg:"noop" (fun _ -> ()) "" end
  ; "yield"      , `Quick, begin fun () -> check ~msg:"yield" yield "" end
  ; "write", `Quick, begin fun () ->
      check ~msg:"string" (fun t -> write_string t "") "";
      check ~msg:"bytes"  (fun t -> write_bytes t empty_bytes) ""
  end
  ; "schedule", `Quick, begin fun () ->
      check ~msg:"string"    (fun t -> schedule_string    t ""             ) "";
      check ~msg:"bytes"     (fun t -> schedule_bytes     t empty_bytes    ) "";
      check ~msg:"bigstring" (fun t -> schedule_bigstring t empty_bigstring) ""
  end ]

let write =
  [ "single", `Quick, begin fun () ->
      let test_bytes = Bytes.unsafe_of_string "test" in
      check ~msg:"string" (fun t -> write_string t "test") "test";
      check ~msg:"bytes"  (fun t -> write_bytes  t test_bytes) "test";
      check ~msg:"char"   (fun t -> write_char   t 'A') "A"
  end ]

let schedule =
  [ "single", `Quick, begin fun () ->
      let test_bytes = Bytes.unsafe_of_string "test" in
      let test_bigstring = bigstring_of_string "test" in
      check ~msg:"string"    (fun t -> schedule_string t "test") "test";
      check ~msg:"bytes"     (fun t -> schedule_bytes t test_bytes) "test";
      check ~msg:"bigstring" (fun t -> schedule_bigstring t test_bigstring) "test"
  end ]

let () =
  Alcotest.run "test suite"
    [ "empty output", empty
    ; "write"       , write
    ; "schedule"    , schedule ]
