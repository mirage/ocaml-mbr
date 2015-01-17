(* Copyright (C) 2014, Thomas Leonard
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt

module A = Bigarray.Array1

type +'a io = 'a Lwt.t
type t = Cstruct.t
type id = unit
type error = [
  | `Unknown of string (** an undiagnosed error *)
  | `Unimplemented     (** operation not yet implemented in the code *)
  | `Is_read_only      (** you cannot write to a read/only instance *)
  | `Disconnected      (** the device has been previously disconnected *)
]

let sector_size = 4096

let error_message = function
  | `Unknown msg -> msg
  | `Unimplemented -> "Unimplemented"
  | `Is_read_only -> "Is_read_only"
  | `Disconnected -> "Disconnected"

type info = {
  read_write: bool;    (** True if we can write, false if read/only *)
  sector_size: int;    (** Octets per sector *)
  size_sectors: int64; (** Total sectors per device *)
}

let safe_of_int64 i64 =
  let i = Int64.to_int i64 in
  assert (Int64.of_int i = i64);
  i

let write device sector_start buffers =
  let rec loop dstoff = function
    | [] -> ()
    | x :: xs ->
        Cstruct.blit x 0 device dstoff (Cstruct.len x);
        loop (dstoff + (Cstruct.len x)) xs in
  loop (safe_of_int64 sector_start * sector_size) buffers;
  `Ok () |> return

let read device sector_start buffers =
  let rec loop dstoff = function
    | [] -> ()
    | x :: xs ->
        Cstruct.blit device dstoff x 0 (Cstruct.len x);
        loop (dstoff + (Cstruct.len x)) xs in
  loop (safe_of_int64 sector_start * sector_size) buffers;
  `Ok () |> return

let info = {
  read_write = true;
  sector_size;
  size_sectors = 64L;
}

let size = info.sector_size * safe_of_int64 info.size_sectors

let get_info _device = return info

let disconnect _device = return ()

let id _device = ()

type page_aligned_buffer = Cstruct.t

let connect () =
  let data = Cstruct.create size in
  Cstruct.blit_from_string (String.make sector_size (Char.chr 0)) 0 data 0 sector_size;
  `Ok data |> return
