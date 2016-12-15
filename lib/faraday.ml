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

type buffer =
  [ `String    of string
  | `Bytes     of Bytes.t
  | `Bigstring of bigstring ]

type 'a iovec =
  { buffer : 'a
  ; off : int
  ; len : int }

module Deque(T:sig type t val sentinel : t end) : sig
  type elem = T.t

  type t

  val sentinel : elem

  val create : int -> t

  val is_empty : t -> bool

  val enqueue : elem -> t -> unit
  val dequeue_exn : t -> elem
  val dequeue : t -> elem option
  val enqueue_front : elem -> t -> unit

  val map_to_list : t -> f:(elem -> 'b) -> 'b list
end = struct
  type elem = T.t

  type t =
    { mutable elements : elem array
    ; mutable front    : int
    ; mutable back     : int }

  let sentinel = T.sentinel

  let create size =
    { elements = Array.make size sentinel; front = 0; back = 0 }

  let is_empty t =
    t.front = t.back

  let ensure_space t =
    if t.back = Array.length t.elements - 1 then begin
      let len = t.back - t.front in
      if t.front > 0 then begin
        (* Shift everything to the front of the array and then clear out
         * dangling pointers to elements from their previous locations. *)
        Array.blit t.elements t.front t.elements 0 len;
        Array.fill t.elements len t.front sentinel
      end else begin
        let old  = t.elements in
        let new_ = Array.(make (2 * length old) sentinel) in
        Array.blit old t.front t.elements 0 len;
        t.elements <- new_
      end;
      t.front <- 0;
      t.back <- len
    end

  let enqueue e t =
    ensure_space t;
    t.elements.(t.back) <- e;
    t.back <- t.back + 1

  let dequeue_exn t =
    if is_empty t then
      raise Not_found
    else
      let result = t.elements.(t.front) in
      t.elements.(t.front) <- sentinel;
      t.front <- t.front + 1;
      result

  let dequeue t =
    try Some (dequeue_exn t) with Not_found -> None

  let enqueue_front e t =
    (* This is in general not true for Deque data structures, but the usage
     * below ensures that there is always space to push an element back on the
     * front. A [enqueue_front] is always preceded by a [dequeue], with no
     * intervening operations. *)
    assert (t.front > 0);
    t.front <- t.front - 1;
    t.elements.(t.front) <- e

  let map_to_list t ~f =
    let result = ref [] in
    for i = t.back - 1 downto t.front do
      result := f t.elements.(i) :: !result
    done;
    !result
end

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

module Buffers = Deque(struct
  type t = buffer iovec
  let sentinel =
    { buffer = `String "\222\173\190\239"; off = 0; len = 4 }
end)
module Flushes = Deque(struct
  type t = int * (unit -> unit)
  let sentinel = 0, fun () -> ()
end)

type t =
  { buffer                 : bigstring
  ; mutable scheduled_pos  : int
  ; mutable write_pos      : int
  ; scheduled              : Buffers.t
  ; flushed                : Flushes.t
  ; mutable bytes_received : int
  ; mutable bytes_written  : int
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
  ; write_pos       = 0
  ; scheduled_pos   = 0
  ; scheduled       = Buffers.create 4
  ; flushed         = Flushes.create 1
  ; bytes_received  = 0
  ; bytes_written   = 0
  ; closed          = false
  ; yield           = false }

let create size =
  of_bigstring (Bigarray.(Array1.create char c_layout size))

let writable t =
  if t.closed then
    failwith "cannot write to closed writer"

let schedule_iovec t ?(off=0) ~len buffer =
  t.bytes_received <- t.bytes_received + len;
  Buffers.enqueue (IOVec.create buffer ~off ~len) t.scheduled

let flush_buffer t =
  let len = t.write_pos - t.scheduled_pos in
  if len > 0 then begin
    let off = t.scheduled_pos in
    t.scheduled_pos <- t.write_pos;
    schedule_iovec t ~off ~len (`Bigstring t.buffer)
  end

let flush t f =
  flush_buffer t;
  if Buffers.is_empty t.scheduled then f ()
  else Flushes.enqueue (t.bytes_received, f) t.flushed

let free_bytes_in_buffer t =
  let buf_len = Bigarray.Array1.dim t.buffer in
  buf_len - t.write_pos

let sufficient_space t to_write =
  free_bytes_in_buffer t >= to_write

let bigarray_to_string ~off ~len src =
  String.init (len - off) (fun i ->
    Bigarray.Array1.unsafe_get src (off + i))

let bigarray_blit src src_off dst dst_off len =
  Bigarray.Array1.(blit (sub src src_off len) (sub dst dst_off len))

let bigarray_blit_from_string src src_off dst dst_off len =
  (* XXX(seliopou): Use Cstruct to turn this into a [memcpy]. *)
  for i = 0 to len - 1 do
    Bigarray.Array1.unsafe_set dst
      (dst_off + i) (String.unsafe_get src (src_off + i))
  done

let bigarray_blit_from_bytes src src_off dst dst_off len =
  (* XXX(seliopou): Use Cstruct to turn this into a [memcpy]. *)
  for i = 0 to len - 1 do
    Bigarray.Array1.unsafe_set dst
      (dst_off + i) (Bytes.unsafe_get src (src_off + i))
  done

let schedule_gen t ~length ~to_buffer ?(off=0) ?len a =
  writable t;
  flush_buffer t;
  let len =
    match len with
    | None     -> length a - off
    | Some len -> len
  in
  schedule_iovec t ~off ~len (to_buffer a)

let schedule_string =
  let to_buffer a = `String a in
  let length      = String.length in
  fun t ?(off=0) ?len a -> schedule_gen t ~length ~to_buffer ~off ?len a

let schedule_bytes =
  let to_buffer a = `Bytes a in
  let length      = Bytes.length in
  fun t ?(off=0) ?len a -> schedule_gen t ~length ~to_buffer ~off ?len a

let schedule_bigstring =
  let to_buffer a = `Bigstring a in
  let length      = Bigarray.Array1.dim in
  fun t ?(off=0) ?len a -> schedule_gen t ~length ~to_buffer ~off ?len a

let write_gen t ~length ~blit ~schedule ?(off=0) ?len a =
  writable t;
  let len =
    match len with
    | None     -> length a - off
    | Some len -> len
  in
  if sufficient_space t len then begin
    blit a off t.buffer t.write_pos len;
    t.write_pos <- t.write_pos + len
  end else
    schedule t ~off ~len a

let write_string =
  let length   = String.length in
  let blit     = bigarray_blit_from_string in
  let schedule t ~off ~len a = schedule_string t ~off ~len a in
  fun t ?(off=0) ?len a -> write_gen t ~length ~blit ~schedule ~off ?len a

let write_bytes =
  let length = Bytes.length in
  let blit   = bigarray_blit_from_bytes in
  let schedule t ~off ~len a = schedule_string t ~off ~len (Bytes.to_string a) in
  fun t ?(off=0) ?len a -> write_gen t ~length ~blit ~schedule ~off ?len a

let write_bigstring =
  let length = Bigarray.Array1.dim in
  let blit   = bigarray_blit in
  let schedule t ~off ~len a =
    let copy = bigarray_to_string ~off ~len a in
    schedule_string t ~off ~len copy
  in
  fun t ?(off=0) ?len a -> write_gen t ~length ~blit ~schedule ~off ?len a

let write_char =
  let length a = assert false in
  let blit src src_off dst dst_off len =
    assert (src_off = 0);
    assert (len = 1);
    Bigarray.Array1.unsafe_set dst dst_off src
  in
  let schedule t ~off ~len a =
    assert (off = 0);
    assert (len = 1);
    schedule_string t ~off:0 ~len:1 (String.make 1 a)
  in
  fun t a -> write_gen t ~length ~blit ~schedule ~off:0 ~len:1 a

let close t =
  t.closed <- true;
  flush_buffer t

let is_closed t =
  t.closed

let has_pending_output t =
  not (Buffers.is_empty t.scheduled)

let yield t =
  t.yield <- true

let rec shift_buffers t written =
  try
    let { len } as iovec = Buffers.dequeue_exn t.scheduled in
    if len <= written then begin
      shift_buffers t (written - len)
    end else
      Buffers.enqueue_front (IOVec.shift iovec written) t.scheduled
  with Not_found ->
    assert (written = 0);
    t.scheduled_pos <- 0;
    t.write_pos <- 0

let rec shift_flushes t =
  try
    let (threshold, f) as flush = Flushes.dequeue_exn t.flushed in
    if t.bytes_written >= threshold then begin f (); shift_flushes t end
    else Flushes.enqueue_front flush t.flushed
  with Not_found ->
    ()

let shift t written =
  shift_buffers t written;
  t.bytes_written <- t.bytes_written + written;
  shift_flushes t

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
    let iovecs = Buffers.map_to_list t.scheduled ~f:(fun x -> x) in
    `Writev iovecs
  end

let rec serialize t writev =
  match operation t with
  | `Writev iovecs ->
    begin match writev iovecs with
    | `Ok   n -> shift t n; if not (Buffers.is_empty t.scheduled) then yield t
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
  let rec loop t acc =
    match operation t with
    | `Writev iovecs ->
      let len = IOVec.lengthv iovecs in
      shift t len;
      loop t (len + acc)
    | `Close         -> acc
    | `Yield         -> loop t acc
  in
  fun t -> loop t 0
