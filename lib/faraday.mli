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

(** Serialization primitives built for speed an memory-efficiency.


    Faraday is a library for writing fast and memory-efficient serializers. Its
    core type and related operation gives the user fine-grained control over
    copying and allocation behavior while serializing user-defined types, and
    presents the output in a form that makes it possible to use vectorized
    write operations, such as the [writev][] system call, or any other platform
    or application-specific output APIs.

    A Faraday serializer manages an internal buffer and a queue of output
    buffers. The output bufferes may be a sub range of the serializer's
    internal buffer or one that is user-provided. Buffered writes such as
    {!write_string}, {!write_char}, {!write_bigstring}, etc., copy the source
    bytes into the serializer's internal buffer. Unbuffered writes such as
    {!schedule_string}, {!schedule_bigstring}, etc., on the other hand perform
    no copying. Instead, they enqueue the source bytes into the serializer's
    write queue directly. *)


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

    Serializers manage an internal buffer for coalescing small writes. The size
    of this buffer is determined when the serializer is created and does not
    change throughout the lifetime of that serializer. If the buffer does not
    contain sufficient space to service the buffered writes of the caller, a
    new buffer of the same size will be allocated.  *)

val write_gen : t ->
  length:('a -> int) ->
  blit:('a -> int -> bigstring -> int -> int -> unit) ->
  ?off:int ->
  ?len:int ->
  'a -> unit
(** [write_gen t ~length ~blit ?off ?len x] copies [x] into the serializer's
    internal buffer using the provided [length] and [blit] operations. *)

val write_string : t -> ?off:int -> ?len:int -> string -> unit
(** [write_string t ?off ?len str] copies [str] into the serializer's
    internal buffer. *)

val write_bytes : t -> ?off:int -> ?len:int -> Bytes.t -> unit
(** [write_bytes t ?off ?len bytes] copies [bytes] into the serializer's
    internal buffer. It is safe to modify [bytes] after this call returns. *)

val write_bigstring : t -> ?off:int -> ?len:int -> bigstring -> unit
(** [write_bigstring t ?off ?len bigstring] copies [bigstring] into the
    serializer's internal buffer. It is safe to modify [bytes] after this call
    returns.  *)

val write_gen
  :  t
  -> length:('a -> int)
  -> blit:('a -> int -> bigstring -> int -> int -> unit)
  -> ?off:int
  -> ?len:int
  -> 'a -> unit
(** [write_gen t ~length ~blit ?off ?len x] copies [x] into the serializer's
    internal buffer using the provided [length] and [blit] operations. *)

val write_char : t -> char -> unit
(** [write_char t char] copies [char] into the serializer's internal buffer. *)

val write_uint8 : t -> int -> unit
  (** [write_uint8 t n] copies the lower 8 bits of [n] into the serializer's
      internal buffer. *)


(** Big endian serializers *)
module BE : sig
  val write_uint16 : t -> int -> unit
  (** [write_uint16 t n] copies the lower 16 bits of [n] into the serializer's
      internal buffer in big-endian byte order. *)

  val write_uint32 : t -> int32 -> unit
  (** [write_uint32 t n] copies [n] into the serializer's internal buffer in
      big-endian byte order. *)

  val write_uint48 : t -> int64 -> unit
  (** [write_uint48 t n] copies the lower 48 bits of [n] into the serializer's
      internal buffer in big-endian byte order. *)

  val write_uint64 : t -> int64 -> unit
  (** [write_uint64 t n] copies [n] into the serializer's internal buffer in
      big-endian byte order. *)

  val write_float : t -> float -> unit
  (** [write_float t n] copies the lower 32 bits of [n] into the serializer's
      internal buffer in big-endian byte order. *)

  val write_double : t -> float -> unit
  (** [write_double t n] copies [n] into the serializer's internal buffer in
      big-endian byte order. *)
end


(** Little endian serializers *)
module LE : sig
  val write_uint16 : t -> int -> unit
  (** [write_uint16 t n] copies the lower 16 bits of [n] into the
      serializer's internal buffer in little-endian byte order. *)

  val write_uint32 : t -> int32 -> unit
  (** [write_uint32 t n] copies [n] into the serializer's internal buffer in
      little-endian byte order. *)

  val write_uint48 : t -> int64 -> unit
  (** [write_uint48 t n] copies the lower 48 bits of [n] into the serializer's
      internal buffer in little-endian byte order. *)

  val write_uint64 : t -> int64 -> unit
  (** [write_uint64 t n] copies [n] into the serializer's internal buffer in
      little-endian byte order. *)

  val write_float : t -> float -> unit
  (** [write_float t n] copies the lower 32 bits of [n] into the serializer's
      internal buffer in little-endian byte order. *)

  val write_double : t -> float -> unit
  (** [write_double t n] copies [n] into the serializer's internal buffer in
      little-endian byte order. *)
end


(** {2 Unbuffered Writes} *)

val schedule_bigstring : t -> ?off:int -> ?len:int -> bigstring -> unit
(** [schedule_bigstring t ?free ?off ?len bigstring] schedules [bigstring] to
    be written the next time the serializer surfaces writes to the user.
    [bigstring] is not copied in this process, so [bigstring] should only be
    modified after [t] has been {!flush}ed. *)


(** {2 Control Operations} *)

val yield : t -> unit
(** [yield t] causes [t] to delay surfacing writes to the user, instead
    returning a [`Yield] operation with an associated continuation [k]. This
    gives the serializer an opportunity to collect additional writes before
    sending them to the underlying device, which will increase the write batch
    size. Barring any intervening calls to [yield t], calling the continuation
    [k] will surface writes to the user. *)

val flush : t -> (unit -> unit) -> unit
(** [flush t f] registers [f] to be called when all prior writes have been
    successfully completed. If [t] has no pending writes, then [f] will be
    called immediately. *)

val close : t -> unit
(** [close t] closes [t]. All subsequent write calls will raise, and any
    pending or subsequent {!yield} calls will be ignored. If the serializer has
    any pending writes, user code will have an opportunity to service them
    before it receives the [Close] operation. *)

val is_closed : t -> bool
(** [is_closed t] is [true] if [close] has been called on [t] and [false]
    otherwise. A closed [t] may still have pending output. *)

val shift : t -> int -> unit
(** [shift t n] removes the first [n] bytes in [t]'s write queue. Any scheduled
    buffers that are contained in this span of bytes are [free()]'d, if
    necessary. *)

val drain : t -> int
(** [drain t] removes all pending writes from [t], returning the number of
    bytes that were enqueued to be written and freeing any scheduled
    buffers in the process. *)

val free_bytes_in_buffer : t -> int
(** [free_bytes_in_buffer t] returns the free space, in bytes, of the
    serializer's write buffer. If a {write_*} call has a length that exceeds
    this value, the serializer will allocate a new buffer that will replace the
    serializer's internal buffer for that and subsequent calls. *)

val has_pending_output : t -> bool
(** [has_pending_output t] is [true] if [t]'s output queue is non-empty. It may
    be the case that [t]'s queued output is being serviced by some other thread
    of control, but has not yet completed. *)

val pending_bytes : t -> int
(** [pending_bytes t] is the size of the next write, in bytes, that [t] will
    surface to the caller. *)


(** {2 Running} *)

type 'a iovec =
  { buffer : 'a
  ; off    : int
  ; len    : int }
(** A view into {!iovec.buffer} starting at {!iovec.off} and with length {!iovec.len}. *)

type operation = [
  | `Writev of bigstring iovec list
    (** Write the {iovec}s, reporting the actual number of bytes written by
        calling {shift}. Failure to do so will result in the same bytes being
        surfaced in a [`Writev] operation multiple times. *)
  | `Yield
    (** Yield to other threads of control, waiting for additional output before
        procedding. The method for achieving this is application-specific, but
        once complete, the caller can proceed with serialization by simply
        making another call to {!operation} or {!serialize}. *)
  | `Close
    (** Serialization is complete. No further output will be received. *)
  ]

val operation : t -> operation
(** [operation t] is the next operation that the caller must perform on behalf
    of the serializer [t]. Users should consider using {!serialize} before this
    function. See the documentation for the {!type:operation} type for details
    on how callers should handle these operations. *)

val serialize : t -> (bigstring iovec list -> [`Ok of int | `Closed]) -> [`Yield | `Close]
(** [serialize t writev] sufaces the next operation of [t] to the caller,
    handling a [`Writev] operation with [writev] function and performing an
    additional bookkeeping on the caller's behalf. In the event that [writev]
    indicates a partial write, {!serialize} will call {!yield} on the serializer
    rather than attempting successive [writev] calls. *)

val serialize_to_string : t -> string
(** [serialize_to_string t] runs [t], collecting the output into a string and
    returning it. [serialzie_to_string t] immediately closes [t] and ignores
    any calls to {!yield} on [t]. *)

val serialize_to_bigstring : t -> bigstring
(** [serialize_to_string t] runs [t], collecting the output into a bigstring
    and returning it. [serialzie_to_bigstring t] immediately closes [t] and
    ignores any calls to {!yield} on [t]. *)
