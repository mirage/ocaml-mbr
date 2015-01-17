(*
 * Copyright (C) 2011-2013 Citrix Inc
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

let (--) = Int64.sub

module Make(B: V1_LWT.BLOCK) = struct
  open Lwt

  type page_aligned_buffer = B.page_aligned_buffer
  type 'a io = 'a B.io
  type error = B.error

  type info = {
    read_write : bool;
    sector_size : int;
    size_sectors : int64;
  }

  type id = {
    b: B.t;
    start_sector: int64;
    length_sectors: int64;
  }

  type t = {
    id : id;
    sec_size : int;
  }

  let get_info t =
    B.get_info t.id.b >>= fun info ->
    return {
      read_write = info.B.read_write;
      sector_size = info.B.sector_size;
      size_sectors = info.B.size_sectors;
    }

  let id t = t.id

  let connect id =
    B.get_info id.b >>= fun info ->
    let needed_length = Int64.add id.start_sector id.length_sectors in
    if needed_length > info.B.size_sectors
    then return (`Error (`Unknown (Printf.sprintf "partition is larger than device: %Ld > %Ld" needed_length info.B.size_sectors)))
    else return (`Ok { id; sec_size = info.B.sector_size })

  let disconnect t = return ()

  let rec length t = function
  | [] -> 0L
  | b :: bs ->
      (* Round block sizes up to get a conservative over-estimate of the length for checking. *)
      let len = Cstruct.len b in
      Int64.(add (of_int ((len + t.sec_size - 1) / t.sec_size)) (length t bs))

  let adjust_start name op t start_sector buffers =
    let buffers_len_sectors = length t buffers in
    if start_sector < 0L || start_sector > t.id.length_sectors -- buffers_len_sectors
    then return (`Error (`Unknown (Printf.sprintf "%s %Ld+%Ld out of range" name start_sector buffers_len_sectors)))
    else op t.id.b (Int64.add start_sector t.id.start_sector) buffers
    
  let read = adjust_start "read" B.read
  let write = adjust_start "write" B.write
end
