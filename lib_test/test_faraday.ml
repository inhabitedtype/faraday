open Faraday

module Operation = struct
  type t =
    [ `Writev of Bigstringaf.t iovec list
    | `Yield
    | `Close ]

  let pp_hum fmt t =
    match t with
    | `Yield -> Format.pp_print_string fmt "Yield"
    | `Close -> Format.pp_print_string fmt "Close"
    | `Writev iovecs ->
      let writev_len = List.length iovecs in
      Format.pp_print_string fmt "Writev [";
      List.iteri (fun i { off; len; buffer } ->
        Format.fprintf fmt "%S" (Bigstringaf.substring ~off ~len buffer);
        if i < writev_len - 1 then Format.pp_print_string fmt ", ")
        iovecs;
      Format.pp_print_string fmt "]";
    ;;

  let equal x y =
    match x, y with
    | `Yield, `Yield -> true
    | `Close, `Close -> true
    | `Writev xs, `Writev ys ->
      let to_string { off; len; buffer } = Bigstringaf.substring ~off ~len buffer in
      let xs = List.map to_string xs in
      let ys = List.map to_string ys in
      xs = ys
    | _, _ -> false
  ;;

  let writev ss =
    `Writev
      (List.map (fun s ->
        let len = String.length s in
        { off = 0; len; buffer = Bigstringaf.of_string ~off:0 ~len s })
        ss)
  ;;
end

module Alcotest = struct
  include Alcotest

  let operation : Operation.t testable = testable Operation.pp_hum Operation.equal
end

let test ?(buf_size=0x100) f =
  let t = create buf_size in
  f t;
  operation t
;;

let noop () =
  Alcotest.(check operation) "noop"
    `Yield (test ignore);
;;

let yield () =
  Alcotest.(check operation) "yield"
    `Yield (test yield)
;;

let empty_writes () =
  Alcotest.(check operation) "empty string"
    `Yield (test (fun t -> write_string t ""));
  Alcotest.(check operation) "empty bytes"
    `Yield (test (fun t -> write_bytes t (Bytes.make 0 '\000')));
  Alcotest.(check operation) "empty bigstring"
    `Yield (test (fun t -> write_bigstring t (Bigstringaf.create 0)));
;;

let empty_schedule () =
  Alcotest.(check operation) "empty schedule"
    `Yield (test (fun t -> schedule_bigstring t (Bigstringaf.create 0)));
;;

let empty =
  [ "noop"          , `Quick, noop
  ; "yield"         , `Quick, yield
  ; "empty writes"  , `Quick, empty_writes
  ; "empty schedule", `Quick, empty_schedule
  ]
;;

let endianness () =
  Alcotest.(check operation) "unit16 le"
    (Operation.writev ["\005\000"])
    (test (fun t -> LE.write_uint16 t 5));
  Alcotest.(check operation) "unit16 be"
    (Operation.writev ["\000\005"])
    (test (fun t -> BE.write_uint16 t 5));
;;

let endian =
  [ "endian", `Quick, endianness ]

let write ?buf_size () =
  let check msg f =
  Alcotest.(check operation) msg
    (Operation.writev [ "test" ])
    (test ?buf_size f)
  in
  check "string"    (fun t -> write_string    t "test");
  check "bytes"     (fun t -> write_bytes     t (Bytes.of_string "test"));
  check "bigstring" (fun t -> write_bigstring t (Bigstringaf.of_string ~off:0 ~len:4 "test"))

let char () =
  Alcotest.(check operation) "char"
    (Operation.writev [ "A" ])
    (test (fun t -> write_char t 'A'));
;;

let write_multiple () =
  let f t =
    write_string t "te";
    write_string t "st";
    write_string t "te";
    write_string t "st";
    write_char   t 't';
    write_char   t 'e'
  in
  Alcotest.(check operation) "with room"
    (Operation.writev ["testtestte"])
    (test f);
  Alcotest.(check operation) "with room"
    (Operation.writev ["te"; "st"; "te"; "st"; "te"])
    (test ~buf_size:1 f);
;;

let write =
  [ "char"           , `Quick, char
  ; "single w/ room" , `Quick, (write : unit -> unit)
  ; "single w/o room", `Quick, write ~buf_size:1
  ; "multiple"       , `Quick, write_multiple
  ]

let schedule () =
  let check msg f =
  Alcotest.(check operation) msg
    (Operation.writev ["one"; "two"])
    (test f)
  in
  check "schedule first" (fun t ->
    schedule_bigstring t (Bigstringaf.of_string ~off:0 ~len:3 "one");
    write_string       t "two");
  check "schedule last" (fun t ->
    write_string       t "one";
    schedule_bigstring t (Bigstringaf.of_string ~off:0 ~len:3 "two"));
;;

let schedule =
  [ "single", `Quick, schedule ]

let rec cross xs ys =
  match xs with
  | [] -> []
  | x::xs' -> List.(map (fun y -> [x; y]) ys) @ (cross xs' ys)

let string_of_bigstring b =
  Bigstringaf.substring ~off:0 ~len:(Bigstringaf.length b) b

let serialize_to_bigstring' t =
  serialize_to_bigstring t
  |> string_of_bigstring

let check ?(buf_size=0x100) ?(serialize=serialize_to_string) ~iovecs ~msg ops result =
  let bigstring_of_string str =
    Bigstringaf.of_string ~off:0 ~len:(String.length str) str
  in
  let t = create buf_size in
  List.iter (function
    | `Write_le        i -> LE.write_uint16 t i
    | `Write_be        i -> BE.write_uint16 t i
    | `Write_string    s -> write_string    t s
    | `Write_bytes     s -> write_bytes     t (Bytes.unsafe_of_string s)
    | `Write_bigstring s -> write_bigstring t (bigstring_of_string s)
    | `Write_char      c -> write_char      t c
    | `Schedule_bigstring s -> schedule_bigstring t (bigstring_of_string s)
    | `Yield -> Faraday.yield t)
  ops;
  Alcotest.(check int) "iovec count" iovecs
    (match operation t with
    | `Writev iovecs  -> List.length iovecs
    | _               -> 0);
  Alcotest.(check string) msg result (serialize t)

let interleaved serialize =
  (* XXX(seliopou): Replace with property-based testing. The property should
     really be: Given a string, for any partition of that string and for any
     assignment of writes and schedules on the partition, the output will be
     the same as the input. *)
  [ "write_then_schedule", `Quick, begin fun () ->
    List.iteri (fun i ops ->
      check ~iovecs:2 ~serialize ~msg:(Printf.sprintf "write_then_schedule: %d" i) ops "test")
    (cross
      [`Write_string "te"; `Write_bytes "te"; `Write_bigstring "te"]
      [`Schedule_bigstring "st"]);
    List.iter (fun ops ->
      check ~iovecs:2 ~serialize ~msg:"write_then_schedule: char" ops "test")
    (cross
      [`Write_char 't'; `Write_string "t"; `Write_bytes "t"]
      [`Schedule_bigstring "est"])
  end
  ; "schedule_then_write", `Quick, begin fun () ->
    List.iteri (fun i ops ->
      check ~iovecs:2 ~serialize ~msg:(Printf.sprintf "schedule_then_write: %d" i) ops "stte")
    (cross
      [`Schedule_bigstring "st"]
      [`Write_string "te"; `Write_bytes "te"; `Write_bigstring "te"]);
    List.iter (fun ops ->
      check ~iovecs:2 ~serialize ~msg:"schedule_then_write: char" ops "estt")
    (cross
      [`Schedule_bigstring "est"]
      [`Write_char 't'; `Write_bytes "t"; `Write_string "t"])
  end ]

module Test_drain = struct
  let normal () =
    let t = create 0x100 in
    write_string t "hello";
    close t;
    Alcotest.(check' int) ~msg:"drain" ~expected:5 ~actual:(drain t)
  ;;

  let before_close () =
    let t = create 0x100 in
    write_string t "hello";
    close t;
    Alcotest.(check' int) ~msg:"drain" ~expected:5 ~actual:(drain t)
  ;;

  let nothing_pending () =
    let t = create 0x100 in
    Alcotest.(check' int) ~msg:"drain" ~expected:0 ~actual:(drain t)
  ;;

  let tests =
    [ "normal", `Quick, normal
    ; "before close", `Quick, before_close
    ; "nothing pending", `Quick, nothing_pending
    ]
end

let () =
  Alcotest.run "test suite"
    [ "empty output"                  , empty
    ; "endianness"                    , endian
    ; "write"                         , write
    ; "single schedule"               , schedule
    ; "interleaved calls (string)"    , interleaved serialize_to_string
    ; "interleaved calls (bigstring)" , interleaved serialize_to_bigstring'
    ; "drain"                         , Test_drain.tests
    ]
