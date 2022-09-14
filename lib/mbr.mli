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

module Geometry : sig
  type t = {
    cylinders : int;
    heads : int;
    sectors : int;
  }
  (** Represents a sector address using the cylinder-heads-sectors
      addressing scheme. *)

  val unmarshal: Cstruct.t -> (t, string) result

  val of_lba_size: int64 -> (t, string) result
  (** For LBA addressable disks of < 8GiB, synthesise a plausible
      geometry given a total number of sectors *)

  val to_chs: t -> int64 -> t
  (** Given a geometry and an LBA offset, compute the CHS of the
      offset *)
end

module Partition : sig
  type t = private {
    active: bool;
    (** true means the partition is active, also known as bootable *)

    first_absolute_sector_chs: Geometry.t;
    (** the CHS address of the first data sector. This is only used
        by BIOSes with pre-LBA disks (< 1996). This will not be marshalled. *)

    ty: int;
    (** the advertised filesystem type *)

    last_absolute_sector_chs: Geometry.t;
    (** the CHS address of the last data sector. This is only used
        by BIOSes with pre-LBA disks (< 1996). This will not be marshalled. *)

    first_absolute_sector_lba: int32;
    (** the Logical Block Address (LBA) of the first data sector. This
        is the absolute sector offset of the first data sector. *)

    sectors: int32;
    (** the total number of sectors in the partition *)
  }
  (** a primary partition within the partition table *)

  val sector_start: t -> int64
  (** [sector_start t] is the int64 representation of
      [t.first_absolute_sector_lba]. *)

  val size_sectors: t -> int64
  (** [size_sectors t] is the int64 representation of [t.sectors]. *)

  val make: ?active:bool -> ?ty:int -> int32 -> int32 -> t
  (** [make ?active ?ty start length] creates a partition starting
      at sector [start] and with length [length] sectors. If the
      active flag is set then the partition will be marked
      as active/bootable. If partition type [ty] is given then
      this will determine the advertised filesystem type, by default
      this is set to 6 (FAT16) *)

  val make': ?active:bool -> ?ty:int -> int64 -> int64 -> (t, string) result
  (** [make' ?active ?ty sector_start size_sectors] is [make ?active ?ty
      (Int64.to_int32 sector_start) (Int64.to_int32 size_sectors)] when both
      [sector_start] and [size_sectors] fit in int32. Otherwise [Error _]. *)

  val unmarshal: Cstruct.t -> (t, string) result
end

type t = private {
  bootstrap_code: Cstruct.t * Cstruct.t;
  original_physical_drive: int;
  seconds: int;
  minutes: int;
  hours: int;
  disk_signature: int32;
  partitions: Partition.t list;
}

val make: Partition.t list -> t
(** [make partitions] constructs an MBR given a desired list of
    primary partitions *)

val marshal: Cstruct.t -> t -> unit

val unmarshal: Cstruct.t -> (t, string) result

val sizeof: int

val default_partition_start: int32
(** default sector offset for first partition *)
