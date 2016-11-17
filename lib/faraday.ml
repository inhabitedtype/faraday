(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Deque : sig
  type 'a t

  val create : int -> 'a t

  val is_empty : 'a t -> bool

  val enqueue : 'a -> 'a t -> unit
  val dequeue : 'a t -> 'a option
  val enqueue_front : 'a -> 'a t -> unit

  val map_to_list : 'a t -> f:('a -> 'b) -> 'b list
end = struct
  type 'a t =
    { mutable elements : 'a option array
    ; mutable front    : int
    ; mutable back     : int
    ; mutable size     : int }

  let create size =
    { elements = Array.make size None; front = 0; back = 0; size }

  let is_empty t =
    t.front = t.back

  let ensure_space t =
    if t.back = t.size - 1 then begin
      let len = t.back - t.front in
      if t.front > 0 then begin
        (* Shift everything to the front of the array and then clear out
         * dangling pointers to elements from their previous locations. *)
        Array.blit t.elements t.front t.elements 0 len;
        Array.fill t.elements len t.front None
      end else begin
        let old = t.elements in
        t.size <- t.size * 2;
        t.elements <- Array.make t.size None;
        Array.blit old t.front t.elements 0 len;
      end;
      t.front <- 0;
      t.back <- len
    end

  let enqueue e t =
    ensure_space t;
    t.elements.(t.back) <- Some e;
    t.back <- t.back + 1

  let dequeue t =
    if is_empty t then
      None
    else
      let result = t.elements.(t.front) in
      t.elements.(t.front) <- None;
      t.front <- t.front + 1;
      result

  let enqueue_front e t =
    (* This is in general not true for Deque data structures, but the usage
     * below ensures that there is always space to push a new element back. A
     * [enqueue_front] is always preceded by a [dequeue], with no intervening
     * operations. *)
    assert (t.front > 0);
    t.front <- t.front - 1;
    t.elements.(t.front) <- Some e

  let map_to_list t ~f =
    let result = ref [] in
    for i = t.back - 1 downto t.front do
      match t.elements.(i) with
      | None   -> assert false
      | Some e -> result := f e :: !result
    done;
    !result
end

type buffer =
  [ `String    of string
  | `Bytes     of Bytes.t
  | `Bigstring of bigstring ]

type 'a iovec =
  { buffer : 'a
  ; off : int
  ; len : int }

module IOVec = struct
  type 'a t = 'a iovec

  let create buffer ~off ~len =
    { buffer; off; len }

  let length t =
    t.len

  let shift { buffer; off; len } n =
    assert (n < len);
    { buffer; off = off + n; len = len - n }

  let lengthv ts =
    let rec loop ts acc =
      match ts with
      | []        -> acc
      | iovec::ts -> loop ts (length iovec + acc)
    in
    loop ts 0
end

type free =
  unit -> unit

type t =
  { buffer                 : bigstring
  ; mutable scheduled_pos  : int
  ; mutable write_pos      : int
  ; scheduled              : (buffer iovec * free option) Deque.t
  ; mutable closed         : bool
  ; mutable yield          : bool
  }

type operation = [
  | `Writev of buffer iovec list
  | `Yield
  | `Close
  ]

let of_bigstring buffer =
  { buffer
  ; write_pos     = 0
  ; scheduled_pos = 0
  ; scheduled     = Deque.create 4
  ; closed        = false
  ; yield         = false }

let create size =
  of_bigstring (Bigarray.(Array1.create char c_layout size))

let writable t =
  if t.closed then
    failwith "cannot write to closed writer"

let schedule_iovec t ?free ?(off=0) ~len buffer =
  Deque.enqueue (IOVec.create buffer ~off ~len, free) t.scheduled

let flush_buffer t =
  let len = t.write_pos - t.scheduled_pos in
  if len > 0 then begin
    let off = t.scheduled_pos in
    t.scheduled_pos <- t.write_pos;
    schedule_iovec t ~off ~len (`Bigstring t.buffer)
  end

let free_bytes_to_write t =
  let buf_len = Bigarray.Array1.dim t.buffer in
  buf_len - t.write_pos

let sufficient_space t to_write =
  free_bytes_to_write t >= to_write

let bigarray_to_string ~off ~len src =
  String.init (len - off) (fun i ->
    Bigarray.Array1.unsafe_get src (off + i))

let bigarray_blit dst dst_off src src_off src_len =
  Bigarray.Array1.(blit (sub src src_off src_len) (sub dst dst_off src_len))

let bigarray_blit_from_string dst dst_off src src_off src_len =
  (* XXX(seliopou): Use Cstruct to turn this into a [memcpy]. *)
  for i = 0 to src_len - 1 do
    Bigarray.Array1.unsafe_set dst
      (dst_off + i) (String.unsafe_get src (src_off + i))
  done

let bigarray_blit_from_bytes dst dst_off src src_off src_len =
  (* XXX(seliopou): Use Cstruct to turn this into a [memcpy]. *)
  for i = 0 to src_len - 1 do
    Bigarray.Array1.unsafe_set dst
      (dst_off + i) (Bytes.unsafe_get src (src_off + i))
  done

let schedule_string t ?(off=0) ?len str =
  writable t;
  flush_buffer t;
  let len =
    match len with
    | None -> String.length str - off
    | Some len -> len
  in
  schedule_iovec t ~off ~len (`String str)

let schedule_bytes t ?free ?(off=0) ?len bytes =
  writable t;
  flush_buffer t;
  let len =
    match len with
    | None -> Bytes.length bytes - off
    | Some len -> len
  in
  let free =
    match free with
    | None -> None
    | Some free -> Some (fun () -> free bytes)
  in
  schedule_iovec t ?free ~off ~len (`Bytes bytes)

let schedule_bigstring t ?free ?(off=0) ?len bigstring =
  writable t;
  flush_buffer t;
  let len =
    match len with
    | None -> Bigarray.Array1.dim bigstring - off
    | Some len -> len
  in
  let free =
    match free with
    | None -> None
    | Some free -> Some (fun () -> free bigstring)
  in
  schedule_iovec t ?free ~off ~len (`Bigstring bigstring)


let write_string t ?(off=0) ?len str =
  writable t;
  let len =
    match len with
    | None -> String.length str - off
    | Some len -> len
  in
  if sufficient_space t len then begin
    bigarray_blit_from_string t.buffer t.write_pos str off len;
    t.write_pos <- t.write_pos + len
  end else
    schedule_string t ~off ~len str

let write_bytes t ?(off=0) ?len bytes =
  writable t;
  let len =
    match len with
    | None -> Bytes.length bytes - off
    | Some len -> len
  in
  if sufficient_space t len then begin
    bigarray_blit_from_bytes t.buffer t.write_pos bytes off len;
    t.write_pos <- t.write_pos + len
  end else
    schedule_string t ~off ~len (Bytes.to_string bytes)

let write_bigstring t ?(off=0) ?len bigstring =
  writable t;
  let len =
    match len with
    | None -> Bigarray.Array1.dim bigstring - off
    | Some len -> len
  in
  if sufficient_space t len then begin
    bigarray_blit t.buffer t.write_pos bigstring off len;
    t.write_pos <- t.write_pos + len
  end else
    schedule_string t ~off:0 ~len (bigarray_to_string ~off ~len bigstring)

let write_char t char =
  writable t;
  if sufficient_space t 1 then begin
    Bigarray.Array1.unsafe_set t.buffer t.write_pos char;
    t.write_pos <- t.write_pos + 1
  end else
    schedule_string t (String.make 1 char)

let close t =
  t.closed <- true;
  flush_buffer t

let is_closed t =
  t.closed

let has_pending_output t =
  not (Deque.is_empty t.scheduled)

let yield t =
  t.yield <- true

let rec shift t written =
  match Deque.dequeue t.scheduled with
  | None               ->
    assert (written = 0);
    t.scheduled_pos <- 0;
    t.write_pos <- 0
  | Some (iovec, free) ->
    if iovec.len <= written then begin
      begin match free with
      | None -> ()
      | Some free -> free ()
      end;
      shift t (written - iovec.len)
    end else
      Deque.enqueue_front (IOVec.shift iovec written, free) t.scheduled

let operation t =
  if t.closed then begin
    t.yield <- false
  end;
  flush_buffer t;
  let nothing_to_do = not (has_pending_output t) in
  if t.closed && nothing_to_do then
    `Close
  else if t.yield || nothing_to_do then begin
    t.yield <- false;
    `Yield
  end else begin
    let iovecs = Deque.map_to_list t.scheduled ~f:fst in
    `Writev iovecs
  end

let rec serialize t writev =
  match operation t with
  | `Writev iovecs ->
    let len = IOVec.lengthv iovecs in
    begin match writev iovecs with
    | `Ok   n -> shift t n; if n < len then yield t
    | `Closed -> close t
    end;
    serialize t writev
  | (`Close|`Yield) as next -> next

let serialize_to_string t =
  close t;
  match operation t with
  | `Writev iovecs ->
    let len = IOVec.lengthv iovecs in
    let bytes = Bytes.create len in
    let pos = ref 0 in
    List.iter (function
      | { buffer = `String buf; off; len } ->
        Bytes.blit_string buf off bytes !pos len;
        pos := !pos + len
      | { buffer = `Bytes  buf; off; len } ->
        Bytes.blit buf off bytes !pos len;
        pos := !pos + len
      | { buffer = `Bigstring buf; off; len } ->
        for i = off to len - 1 do
          Bytes.unsafe_set bytes (!pos + i) (Bigarray.Array1.unsafe_get buf i)
        done;
        pos := !pos + len)
    iovecs;
    shift t len;
    assert (operation t = `Close);
    Bytes.unsafe_to_string bytes
  | `Close -> ""
  | `Yield -> assert false

let drain =
  let writev iovecs = `Ok (IOVec.lengthv iovecs) in
  let rec loop t =
    match serialize t writev with
    | `Close -> ()
    | `Yield -> loop t
  in
  loop
