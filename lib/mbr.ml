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

  let unmarshal buf : (t, _) result =
    (if Cstruct.length buf < 3 then
     Error (Printf.sprintf "geometry too small: %d < %d" (Cstruct.length buf) 3)
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

  let make ?(active = false) ?(ty = 6) first_absolute_sector_lba sectors =
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

  let make' ?active ?ty sector_start size_sectors =
    if
      Int64.(
        logand sector_start 0xFFFF_FFFFL = sector_start
        && logand size_sectors 0xFFFF_FFFFL = size_sectors)
    then
      make ?active ?ty
        (Int64.to_int32 sector_start)
        (Int64.to_int32 size_sectors)
    else Error "partition parameters do not fit in int32"

  [%%cstruct
  type part = {
    status : uint8_t;
    first_absolute_sector_chs : uint8_t; [@len 3]
    ty : uint8_t;
    last_absolute_sector_chs : uint8_t; [@len 3]
    first_absolute_sector_lba : uint32_t;
    sectors : uint32_t;
  }
  [@@little_endian]]

  let _ = assert (sizeof_part = 16)
  let sizeof = sizeof_part

  let unmarshal buf =
    (if Cstruct.length buf < sizeof_part then
     Error
       (Printf.sprintf "partition entry too small: %d < %d" (Cstruct.length buf)
          sizeof_part)
    else Ok ())
    >>= fun () ->
    let buf = Cstruct.sub buf 0 sizeof_part in
    let ty = get_part_ty buf in
    if ty == 0x00 then
      if Cstruct.for_all (( = ) '\000') buf then Ok None
      else Error "Non-zero empty partition type"
    else
      let active = get_part_status buf = 0x80 in
      Geometry.unmarshal (get_part_first_absolute_sector_chs buf)
      >>= fun first_absolute_sector_chs ->
      Geometry.unmarshal (get_part_last_absolute_sector_chs buf)
      >>= fun last_absolute_sector_chs ->
      let first_absolute_sector_lba = get_part_first_absolute_sector_lba buf in
      let sectors = get_part_sectors buf in
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
    set_part_status buf (if t.active then 0x80 else 0);
    set_part_ty buf t.ty;
    set_part_first_absolute_sector_lba buf t.first_absolute_sector_lba;
    set_part_sectors buf t.sectors
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
let make ?disk_signature partitions =
  let signature_msg = match disk_signature with
    | Some signature -> Printf.sprintf "Disk signature is: %ld\n" signature
    | None -> "No disk signature provided\n" in
  print_endline signature_msg;
  
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
      disk_signature= (match disk_signature with Some s -> s | None -> 0l);
      partitions;
    }

(* "modern standard" MBR from wikipedia: *)
[%%cstruct
type mbr = {
  bootstrap_code1 : uint8_t; [@len 218]
  _zeroes_1 : uint8_t; [@len 2]
  original_physical_drive : uint8_t;
  seconds : uint8_t;
  minutes : uint8_t;
  hours : uint8_t;
  bootstrap_code2 : uint8_t; [@len 216]
  disk_signature : uint32_t;
  _zeroes_2 : uint8_t; [@len 2]
  partitions : uint8_t; [@len 64]
  signature1 : uint8_t; (* 0x55 *)
  signature2 : uint8_t; (* 0xaa *)
}
[@@little_endian]]

let _ = assert (sizeof_mbr = 512)

let unmarshal (buf : Cstruct.t) : (t, string) result =
  (if Cstruct.length buf < sizeof_mbr then
   Error
     (Printf.sprintf "MBR too small: %d < %d" (Cstruct.length buf) sizeof_mbr)
  else Ok ())
  >>= fun () ->
  let signature1 = get_mbr_signature1 buf in
  let signature2 = get_mbr_signature2 buf in
  (if signature1 = 0x55 && signature2 = 0xaa then Ok ()
  else
    Error
      (Printf.sprintf "Invalid signature: %02x %02x <> 0x55 0xaa" signature1
         signature2))
  >>= fun () ->
  let bootstrap_code =
    Cstruct.append (get_mbr_bootstrap_code1 buf) (get_mbr_bootstrap_code2 buf)
  in
  let bootstrap_code = Cstruct.to_string bootstrap_code in
  let original_physical_drive = get_mbr_original_physical_drive buf in
  let seconds = get_mbr_seconds buf in
  let minutes = get_mbr_minutes buf in
  let hours = get_mbr_hours buf in
  let disk_signature = get_mbr_disk_signature buf in
  let partitions = get_mbr_partitions buf in
  Partition.unmarshal (Cstruct.shift partitions (0 * Partition.sizeof))
  >>= fun p1 ->
  Partition.unmarshal (Cstruct.shift partitions (1 * Partition.sizeof))
  >>= fun p2 ->
  Partition.unmarshal (Cstruct.shift partitions (2 * Partition.sizeof))
  >>= fun p3 ->
  Partition.unmarshal (Cstruct.shift partitions (3 * Partition.sizeof))
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
  let bootstrap_code1 = String.sub t.bootstrap_code 0 218
  and bootstrap_code2 = String.sub t.bootstrap_code 218 216 in
  set_mbr_bootstrap_code1 bootstrap_code1 0 buf;
  set_mbr_bootstrap_code2 bootstrap_code2 0 buf;
  set_mbr_original_physical_drive buf t.original_physical_drive;
  set_mbr_seconds buf t.seconds;
  set_mbr_minutes buf t.minutes;
  set_mbr_hours buf t.hours;
  set_mbr_disk_signature buf t.disk_signature;
  let partitions = get_mbr_partitions buf in
  let _ =
    List.fold_left
      (fun buf p ->
        Partition.marshal buf p;
        Cstruct.shift buf Partition.sizeof)
      partitions t.partitions
  in
  set_mbr_signature1 buf 0x55;
  set_mbr_signature2 buf 0xaa

let sizeof = sizeof_mbr
let default_partition_start = 2048l
