(*
 * Copyright (C) 2013 Citrix Inc
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

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let ( >>= ) x f = match x with
  | Error y -> Error y
  | Ok z -> f z

let return x = Ok x
let fail y = Error y

module Geometry = struct
  type t = {
    cylinders : int;
    heads : int;
    sectors : int;
  }

  let unmarshal buf =
    ( if Cstruct.len buf < 3
      then fail (Printf.sprintf "geometry too small: %d < %d" (Cstruct.len buf) 3)
      else return () ) >>= fun () ->
    let heads = Cstruct.get_uint8 buf 0 in
    let y = Cstruct.get_uint8 buf 1 in
    let z = Cstruct.get_uint8 buf 2 in
    let sectors = y land 0b0111111 in
    let cylinders = (y lsl 2) lor z in
    return { cylinders; heads; sectors }
end

module Partition = struct
  type t = {
    active: bool;
    first_absolute_sector_chs: Geometry.t;
    ty: int;
    last_absolute_sector_chs: Geometry.t;
    first_absolute_sector_lba: int32;
    sectors: int32;
  }

  cstruct part {
    uint8_t status;
    uint8_t first_absolute_sector_chs[3];
    uint8_t ty;
    uint8_t last_absolute_sector_chs[3];
    uint32_t first_absolute_sector_lba;
    uint32_t sectors

  } as little_endian

  let _ = assert (sizeof_part = 16)

  let unmarshal (buf: Cstruct.t) : (t, string) result =
    ( if Cstruct.len buf < sizeof_part
      then fail (Printf.sprintf "partition entry too small: %d < %d" (Cstruct.len buf) sizeof_part)
      else return () ) >>= fun () ->
    let active = get_part_status buf = 0x80 in
    Geometry.unmarshal (get_part_first_absolute_sector_chs buf) >>= fun first_absolute_sector_chs ->
    let ty = get_part_ty buf in
    Geometry.unmarshal (get_part_last_absolute_sector_chs buf) >>= fun last_absolute_sector_chs ->
    let first_absolute_sector_lba = get_part_first_absolute_sector_lba buf in
    let sectors = get_part_sectors buf in
    return { active; first_absolute_sector_chs; ty;
      last_absolute_sector_chs; first_absolute_sector_lba;
      sectors }
end

type t = {
  bootstrap_code: Cstruct.t * Cstruct.t;
  original_physical_drive: int;
  seconds: int;
  minutes: int;
  hours: int;
  disk_signature: int32;
  partitions: Partition.t list;
}

(* "modern standard" MBR from wikipedia: *)
cstruct mbr {
  uint8_t bootstrap_code1[218];
  uint8_t _zeroes_1[2];
  uint8_t original_physical_drive;
  uint8_t seconds;
  uint8_t minutes;
  uint8_t hours;
  uint8_t bootstrap_code2[216];
  uint32_t disk_signature;
  uint8_t _zeroes_2[2];
  uint8_t partition1[16];
  uint8_t partition2[16];
  uint8_t partition3[16];
  uint8_t partition4[16];
  uint8_t signature1; (* 0x55 *)
  uint8_t signature2  (* 0xaa *)
} as little_endian

let _ = assert(sizeof_mbr = 512)

let unmarshal (buf: Cstruct.t) : (t, string) result =
    ( if Cstruct.len buf < sizeof_mbr
      then fail (Printf.sprintf "MBR too small: %d < %d" (Cstruct.len buf) sizeof_mbr)
      else return () ) >>= fun () ->
    let signature1 = get_mbr_signature1 buf in
    let signature2 = get_mbr_signature2 buf in
    ( if signature1 = 0x55 && (signature2 = 0xaa)
      then return ()
      else fail (Printf.sprintf "Invalid signature: %02x %02x <> 0x55 0xaa" signature1 signature2) ) >>= fun () ->
    let bootstrap_code = get_mbr_bootstrap_code1 buf, get_mbr_bootstrap_code2 buf in
    let original_physical_drive = get_mbr_original_physical_drive buf in
    let seconds = get_mbr_seconds buf in
    let minutes = get_mbr_minutes buf in
    let hours = get_mbr_hours buf in
    let disk_signature = get_mbr_disk_signature buf in
    Partition.unmarshal (get_mbr_partition1 buf) >>= fun p1 ->
    Partition.unmarshal (get_mbr_partition2 buf) >>= fun p2 ->
    Partition.unmarshal (get_mbr_partition3 buf) >>= fun p3 ->
    Partition.unmarshal (get_mbr_partition4 buf) >>= fun p4 ->
    let partitions = [ p1; p2; p3; p4 ] in
    return { bootstrap_code;
      original_physical_drive; seconds; minutes; hours;
      disk_signature;
      partitions }
