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

(** Serialization primitives built for speed an memory-efficiency. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t
(** The type of a serializer. *)


(** {2 Constructors} *)

val create : int -> t
(** [create len] creates a serializer with a fixed-length internal buffer of
    length [len]. *)

val of_bigstring : bigstring -> t
(** [of_bigstring buf] creates a serializer, using [buf] as its internal
    buffer. The serializer takes ownership of [buf] until the serializer has
    been closed and flushed of all output. *)


(** {2 Buffered Writes}

    Serializers manage an internal buffer for batching small writes. The size
    of this buffer is determined when the serializer is created and does not
    change throughout the lifetime of that serializer. If the buffer does not
    contain sufficient space to service the buffered writes of the caller, it
    will cease to batch writes and begin to allocate in certain situations. See
    the documentation for the subsequent {write_*} calls and
    {!free_bytes_to_write} for additional details. *)

val write_string : t -> ?off:int -> ?len:int -> string -> unit
(** [write_string t ?off ?len str] copies [str] into the serializer's
    internal buffer, if possible. In the event that there is not sufficient
    space in the buffer, [str] will be scheduled for the next batch write, and
    should therefore be treated as immutable. *)

val write_bytes : t -> ?off:int -> ?len:int -> Bytes.t -> unit
(** [write_bytes t ?off ?len bytes] copies [bytes] into the serializer's
    internal buffer, if possible. In the event that there is not sufficient
    space in the buffer, [bytes] will be copied, and that sopy will be
    scheduled for the next batch write. In either case, it is safe to modify
    [bytes] after this call returns. *)

val write_bigstring : t -> ?off:int -> ?len:int -> bigstring -> unit
(** [write_bigstring t ?off ?len bigstring] copies [bigstring] into the
    serializer's internal buffer, if possible. In the event that there is not
    sufficient space in the buffer, [bigstring] will be copied, and that sopy
    will be scheduled for the next batch write. In either case, it is safe to
    modify [bytes] after this call returns.  *)

val write_char : t -> char -> unit
(** [write_char t char] copies [char] into the serializer's internal buffer, if
    possible. *)


(** {2 Unbuffered Writes} *)

val schedule_string : t -> ?off:int -> ?len:int -> string -> unit
(** [schedule_string t ?off ?len str] schedules [str] to be written the next
    time the serializer surfaces writes to the user. [str] is not copied in
    this process. *)

val schedule_bytes
  :   t
  -> ?free:(Bytes.t -> unit)
  -> ?off:int
  -> ?len:int
  -> Bytes.t
  -> unit
  (** [schedule_bytes t ?free ?off ?len bytes] schedules [bytes] to be written
      the next time the serializer surfaces writes to the user. [bytes] is not
      copied in this process, so [bytes] should only be modified after the
      [free] function has been called on [bytes], if provided. *)

val schedule_bigstring
  :  t
  -> ?free:(bigstring -> unit)
  -> ?off:int
  -> ?len:int
  -> bigstring
  -> unit
  (** [schedule_bigstring t ?free ?off ?len bigstring] schedules [bigstring] to
      be written the next time the serializer surfaces writes to the user.
      [bigstring] is not copied in this process, so [bigstring] should only be
      modified after the [free] function has been called on [bigstring], if
      provided. *)


(** {2 Control Operations} *)

val yield : t -> unit
(** [yield t] causes the serializer to delay surfacing writes to the user,
    instead returning a {!Yield} operation with an associated continuation [k].
    This gives the serializer an opportunity to collect additional writes
    before sending them to the underlying device, which will increase the write
    batch size. Barring any intervening calls to [yield t], calling the
    continuation [k] will surface writes to the user. *)

val close : t -> unit
(** [close t] closes the serializer. All subsequent write calls will raise, and
    any pending or subsequent {yield} calls will be ignored. If the serializer
    has any pending writes, user code will have an opportunity to service them
    before it receives the {Close} operation. *)

val free_bytes_to_write : t -> int
(** [free_bytes_to_write t] returns the free space, in bytes, of the
    serializer's write buffer. If a call to {!write_bytes} or {!write_char} has
    a length that exceeds the serializer's free size, the serializer will
    allocate an additional buffer, copy the contents of the write call into
    that buffer, and schedule it as a separate {!iovec}. If a call to
    {!write_string} has a length that exceeds the serializer's free size, the
    serializer will schedule it as {!iovec}. *)


(** {2 Running} *)

type buffer =
  [ `String    of string
  | `Bytes     of Bytes.t
  | `Bigstring of bigstring ]

type 'a iovec =
  { buffer : 'a
  ; off : int
  ; len : int }
(** A view into {buffer} starting at {off} and with length {len}. *)

type op =
  | Writev of buffer iovec list * (int  -> op)
    (** Write the {iovec}s, passing the continuation the number of bytes
        successfully written. *)
  | Yield  of                     (unit -> op)
    (** Yield to other threads of control, whether logical or actual, and wait
        for additional output before procedding. The method for achieving this
        is application-specific. It is safe to call the continuation even no
        additional output has been received. *)
  | Close
    (** Serialization is complete. No further output will be received. *)

val serialize : t -> op
(** [serialize t] runs [t], surfacing operations to the user together with an
    explicit continuation. The application may determine when to call the
    continuations in the {Writev} and {Yield} cases, with the caveat that the
    [int] passed to the continuation in the {Writev} correspond to the number
    of bytes that have {i actually} been written from the {iovec}s at the time
    that the user invokes the continuation. *)

val serialize_to_string : t -> string
(** [serialize_to_string t] runs [t], collecting the output into a string and
    returning it. [t] is immediately closed, and all calls to {yield} are
    ignored. *)

val drain : t -> unit
(** [drain t] removes all pending writes from [t], freeing any scheduled
    buffers in the process.  *)
