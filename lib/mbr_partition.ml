(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

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

  type t = id

  let get_info t =
    B.get_info t.b >>= fun info ->
    return {
      read_write = info.B.read_write;
      sector_size = info.B.sector_size;
      size_sectors = info.B.size_sectors;
    }

  let id t = t

  let connect id =
    B.get_info id.b >>= fun info ->
    let needed_length = Int64.add id.start_sector id.length_sectors in
    if needed_length > info.B.size_sectors
    then return (`Error (`Unknown (Printf.sprintf "partition is larger than device: %Ld > %Ld" needed_length info.B.size_sectors)))
    else return (`Ok id)

  let disconnect t = return ()

  let rec length = function
  | [] -> 0L
  | b :: bs -> Int64.(add (of_int (Cstruct.len b / 512)) (length bs))
    
  let read t start_sector buffers =
    let length = Int64.add start_sector (length buffers) in
    if length > t.length_sectors
    then return (`Error (`Unknown (Printf.sprintf "read %Ld %Ld out of range" start_sector length)))
    else B.read t.b (Int64.add start_sector t.start_sector) buffers

  let write t start_sector buffers =
    let length = Int64.add start_sector (length buffers) in
    if length > t.length_sectors
    then return (`Error (`Unknown (Printf.sprintf "write %Ld %Ld out of range" start_sector length)))
    else B.write t.b (Int64.add start_sector t.start_sector) buffers
end
