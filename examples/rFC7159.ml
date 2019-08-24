open Faraday

type json =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of float
  | `Object of (string * json) list
  | `Array of json list ]

let to_hex_digit i =
  Char.unsafe_chr (if i < 10 then i + 48 else i + 87)

let serialize_string t s =
  (* TODO: Implement proper unicode verification. *)
  let flush ~off ~len = 
    if len <> 0 then write_string t ~off ~len s in
  let rec go ~off ~len = 
    if String.length s = off + len 
    then flush ~off ~len
    else
      let i = off + len in
      match String.get s i with
      | c when c <= '\031' -> (* non-visible characters have to be escaped *)
        let i = Char.code c in
        flush ~off ~len;
        write_string t "\\u00"; 
        write_char   t (to_hex_digit (i lsr 4));
        write_char   t (to_hex_digit (i land 0xf));
        go ~off:(i+1) ~len:0
      | '"'    -> flush ~off ~len; write_string t "\\"  ; go ~off:(i+1) ~len:0
      | '/'    -> flush ~off ~len; write_string t "\\/" ; go ~off:(i+1) ~len:0
      | '\b'   -> flush ~off ~len; write_string t "\\b" ; go ~off:(i+1) ~len:0
      | '\012' -> flush ~off ~len; write_string t "\\f" ; go ~off:(i+1) ~len:0
      | '\n'   -> flush ~off ~len; write_string t "\\n" ; go ~off:(i+1) ~len:0
      | '\r'   -> flush ~off ~len; write_string t "\\r" ; go ~off:(i+1) ~len:0
      | '\t'   -> flush ~off ~len; write_string t "\\t" ; go ~off:(i+1) ~len:0
      | '\\'   -> flush ~off ~len; write_string t "\\\\"; go ~off:(i+1) ~len:0
      | _      -> go ~off ~len:(len + 1)
  in
  write_char t '"';
  go ~off:0 ~len:0;
  write_char t '"'

let serialize_number t f =
  let f = string_of_float f in
  let len = String.length f in
  let len = if String.get f (len - 1) = '.' then len - 1 else len in
  write_string t ~len f

let rec serialize_json t json =
  match json with
  | `Null      -> write_string t "null"
  | `True      -> write_string t "true"
  | `False     -> write_string t "false"
  | `Number n  -> serialize_number t n
  | `String s  -> serialize_string t s
  | `Object [] -> write_string t "{}"
  | `Object ((k, v)::kvs) ->
      write_char t '{';
      serialize_kv t k v;
      List.iter (fun (k, v) ->
        write_char t ',';
        serialize_kv t k v)
      kvs;
      write_char t '}';
  | `Array  [] -> write_string t "[]"
  | `Array (v::vs) ->
    write_char t '[';
    serialize_json t v;
    List.iter (fun v ->
      write_char t ',';
      serialize_json t v)
    vs;
    write_char t ']'

and serialize_kv t k v =
  serialize_string t k;
  write_char       t ':';
  serialize_json   t v
