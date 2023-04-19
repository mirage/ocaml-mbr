(*
 * Copyright (C) 2013 Citrix Inc
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

let ( >>= ) = Result.bind

module Geometry = struct
  type t = { cylinders : int; heads : int; sectors : int }

  let kib = 1024L
  let mib = Int64.mul kib 1024L
  let sizeof = 3

  let unmarshal buf : (t, _) result =
    (if Cstruct.length buf < sizeof then
     Error
       (Printf.sprintf "geometry too small: %d < %d" (Cstruct.length buf) sizeof)
    else Ok ())
    >>= fun () ->
    let heads = Cstruct.get_uint8 buf 0 in
    let y = Cstruct.get_uint8 buf 1 in
    let z = Cstruct.get_uint8 buf 2 in
    let sectors = y land 0b0111111 in
    let cylinders = (y lsl 2) lor z in
    Ok { cylinders; heads; sectors }

  let of_lba_size x =
    let sectors = 63 in
    (if x < Int64.(mul 504L mib) then Ok 16
    else if x < Int64.(mul 1008L mib) then Ok 64
    else if x < Int64.(mul 4032L mib) then Ok 128
    else if x < Int64.(add (mul 8032L mib) (mul 512L kib)) then Ok 255
    else Error (Printf.sprintf "sector count exceeds LBA max: %Ld" x))
    >>= fun heads ->
    let cylinders =
      Int64.(to_int (div (div x (of_int sectors)) (of_int heads)))
    in
    Ok { cylinders; heads; sectors }

  let to_chs g x =
    let open Int64 in
    let cylinders = to_int (div x (mul (of_int g.sectors) (of_int g.heads))) in
    let heads = to_int (rem (div x (of_int g.sectors)) (of_int g.heads)) in
    let sectors = to_int (succ (rem x (of_int g.sectors))) in
    { cylinders; heads; sectors }
end

module Partition = struct
  type t = {
    active : bool;
    first_absolute_sector_chs : Geometry.t;
    ty : int;
    last_absolute_sector_chs : Geometry.t;
    first_absolute_sector_lba : int32;
    sectors : int32;
  }

  let sector_start t =
    Int64.(logand (of_int32 t.first_absolute_sector_lba) 0xFFFF_FFFFL)

  let size_sectors t = Int64.(logand (of_int32 t.sectors) 0xFFFF_FFFFL)

  let make ?(active = false) ~partition_type:(ty : int)
      first_absolute_sector_lba sectors =
    (* ty has to fit in a uint8_t, and ty=0 is reserved for empty partition entries *)
    (if ty > 0 && ty < 256 then Ok ()
    else Error "Mbr.Partition.make: ty must be between 1 and 255")
    >>= fun () ->
    let first_absolute_sector_chs =
      { Geometry.cylinders = 0; heads = 0; sectors = 0 }
    in
    let last_absolute_sector_chs = first_absolute_sector_chs in
    Ok
      {
        active;
        first_absolute_sector_chs;
        ty;
        last_absolute_sector_chs;
        first_absolute_sector_lba;
        sectors;
      }

  let make' ?active ~partition_type:(ty : int) sector_start size_sectors =
    if
      Int64.(
        logand sector_start 0xFFFF_FFFFL = sector_start
        && logand size_sectors 0xFFFF_FFFFL = size_sectors)
    then
      let sector_start = Int64.to_int32 sector_start in
      let size_sectors = Int64.to_int32 size_sectors in
      make ?active ~partition_type:ty sector_start size_sectors
    else Error "partition parameters do not fit in int32"

  let sizeof = 16
  let status_offset = 0
  let first_absolute_sector_chs_offset = 1
  let ty_offset = 4
  let last_absolute_sector_chs_offset = 5
  let first_absolute_sector_lba_offset = 8
  let sectors_offset = 12

  let unmarshal buf =
    (if Cstruct.length buf < sizeof then
     Error
       (Printf.sprintf "partition entry too small: %d < %d" (Cstruct.length buf)
          sizeof)
    else Ok ())
    >>= fun () ->
    let buf = Cstruct.sub buf 0 sizeof in
    let ty = Cstruct.get_uint8 buf ty_offset in
    if ty == 0x00 then
      if Cstruct.for_all (( = ) '\000') buf then Ok None
      else Error "Non-zero empty partition type"
    else
      let active = Cstruct.get_uint8 buf status_offset = 0x80 in
      Geometry.unmarshal
        (Cstruct.sub buf first_absolute_sector_chs_offset Geometry.sizeof)
      >>= fun first_absolute_sector_chs ->
      Geometry.unmarshal
        (Cstruct.sub buf last_absolute_sector_chs_offset Geometry.sizeof)
      >>= fun last_absolute_sector_chs ->
      let first_absolute_sector_lba =
        Cstruct.LE.get_uint32 buf first_absolute_sector_lba_offset
      in
      let sectors = Cstruct.LE.get_uint32 buf sectors_offset in
      Ok
        (Some
           {
             active;
             first_absolute_sector_chs;
             ty;
             last_absolute_sector_chs;
             first_absolute_sector_lba;
             sectors;
           })

  let marshal (buf : Cstruct.t) t =
    Cstruct.set_uint8 buf status_offset (if t.active then 0x80 else 0);
    Cstruct.set_uint8 buf ty_offset t.ty;
    Cstruct.LE.set_uint32 buf first_absolute_sector_lba_offset
      t.first_absolute_sector_lba;
    Cstruct.LE.set_uint32 buf sectors_offset t.sectors
end

type t = {
  bootstrap_code : string;
  original_physical_drive : int;
  seconds : int;
  minutes : int;
  hours : int;
  disk_signature : int32;
  partitions : Partition.t list;
}

let make ?(disk_signature = 0l) partitions =
  (if List.length partitions <= 4 then Ok () else Error "Too many partitions")
  >>= fun () ->
  let num_active =
    List.fold_left
      (fun acc p -> if p.Partition.active then succ acc else acc)
      0 partitions
  in
  (if num_active <= 1 then Ok ()
  else Error "More than one active/boot partitions is not advisable")
  >>= fun () ->
  let partitions =
    List.sort
      (fun p1 p2 ->
        Int32.unsigned_compare p1.Partition.first_absolute_sector_lba
          p2.Partition.first_absolute_sector_lba)
      partitions
  in
  (* Check for overlapping partitions *)
  List.fold_left
    (fun r p ->
      r >>= fun offset ->
      if
        Int32.unsigned_compare offset p.Partition.first_absolute_sector_lba <= 0
      then
        Ok (Int32.add p.Partition.first_absolute_sector_lba p.Partition.sectors)
      else Error "Partitions overlap")
    (Ok 1l) (* We start at 1 so the partitions don't overlap with the MBR *)
    partitions
  >>= fun (_ : int32) ->
  let bootstrap_code = String.init (218 + 216) (Fun.const '\000') in
  let original_physical_drive = 0 in
  let seconds = 0 in
  let minutes = 0 in
  let hours = 0 in

  Ok
    {
      bootstrap_code;
      original_physical_drive;
      seconds;
      minutes;
      hours;
      disk_signature;
      partitions;
    }

(* "modern standard" MBR from wikipedia: *)
let sizeof_mbr = 512
let bootstrap_code1_offset = 0
let bootstrap_code1_len = 218
let _zeroes_1_offset = 218
let _zeroes_1_len = 2
let original_physical_drive_offset = 220
let seconds_offset = 221
let minutes_offset = 222
let hours_offset = 223
let bootstrap_code2_offset = 224
let bootstrap_code2_len = 216
let disk_signature_offset = 440
let _zeroes_2_offset = 444 (* also copy-protected *)
let _zeroes_2_len = 2
let partitions_offset = 446

let partition_offset n =
  assert (n >= 0 && n < 4);
  partitions_offset + (n * Partition.sizeof)

let signature1_offset = 510
let signature2_offset = 511

let unmarshal (buf : Cstruct.t) : (t, string) result =
  (if Cstruct.length buf < sizeof_mbr then
   Error
     (Printf.sprintf "MBR too small: %d < %d" (Cstruct.length buf) sizeof_mbr)
  else Ok ())
  >>= fun () ->
  let signature1 = Cstruct.get_uint8 buf signature1_offset in
  let signature2 = Cstruct.get_uint8 buf signature2_offset in
  (if signature1 = 0x55 && signature2 = 0xaa then Ok ()
  else
    Error
      (Printf.sprintf "Invalid signature: %02x %02x <> 0x55 0xaa" signature1
         signature2))
  >>= fun () ->
  let bootstrap_code =
    Cstruct.copyv
      [
        Cstruct.sub buf bootstrap_code1_offset bootstrap_code1_len;
        Cstruct.sub buf bootstrap_code2_offset bootstrap_code2_len;
      ]
  in
  let original_physical_drive =
    Cstruct.get_uint8 buf original_physical_drive_offset
  in
  let seconds = Cstruct.get_uint8 buf seconds_offset in
  let minutes = Cstruct.get_uint8 buf minutes_offset in
  let hours = Cstruct.get_uint8 buf hours_offset in
  let disk_signature = Cstruct.LE.get_uint32 buf disk_signature_offset in
  Partition.unmarshal (Cstruct.sub buf (partition_offset 0) Partition.sizeof)
  >>= fun p1 ->
  Partition.unmarshal (Cstruct.sub buf (partition_offset 1) Partition.sizeof)
  >>= fun p2 ->
  Partition.unmarshal (Cstruct.sub buf (partition_offset 2) Partition.sizeof)
  >>= fun p3 ->
  Partition.unmarshal (Cstruct.sub buf (partition_offset 3) Partition.sizeof)
  >>= fun p4 ->
  let partitions = List.filter_map Fun.id [ p1; p2; p3; p4 ] in
  Ok
    {
      bootstrap_code;
      original_physical_drive;
      seconds;
      minutes;
      hours;
      disk_signature;
      partitions;
    }

let marshal (buf : Cstruct.t) t =
  Cstruct.blit_from_string t.bootstrap_code 0 buf bootstrap_code1_offset
    bootstrap_code1_len;
  Cstruct.blit_from_string t.bootstrap_code bootstrap_code1_len buf
    bootstrap_code2_offset bootstrap_code2_len;
  Cstruct.set_uint8 buf original_physical_drive_offset t.original_physical_drive;
  Cstruct.set_uint8 buf seconds_offset t.seconds;
  Cstruct.set_uint8 buf minutes_offset t.minutes;
  Cstruct.set_uint8 buf hours_offset t.hours;
  Cstruct.LE.set_uint32 buf disk_signature_offset t.disk_signature;
  List.iteri
    (fun i p ->
      Partition.marshal
        (Cstruct.sub buf (partition_offset i) Partition.sizeof)
        p)
    t.partitions;
  Cstruct.set_uint8 buf signature1_offset 0x55;
  Cstruct.set_uint8 buf signature2_offset 0xaa

let sizeof = sizeof_mbr
let default_partition_start = 2048l
