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
  type t = {
    active: bool;
    (** true means the partition is active, also known as bootable *)

    first_absolute_sector_chs: Geometry.t;
    (** the CHS address of the first data sector. This is only used
        by BIOSes with pre-LBA disks (< 1996) *)

    ty: int;
    (** the advertised filesystem type *)

    last_absolute_sector_chs: Geometry.t;
    (** the CHS address of the last data sector. This is only used
        by BIOSes with pre-LBA disks (< 1996) *)

    first_absolute_sector_lba: int32;
    (** the Logical Block Address (LBA) of the first data sector. This
        is the absolute sector offset of the first data sector. *)

    sectors: int32;
    (** the total number of sectors in the partition *)
  }
  (** a primary partition within the partition table *)

  val make: ?active:bool -> ?ty:int -> int32 -> int32 -> t
  (** [make ?active ?ty start length] creates a partition starting
      at sector [start] and with length [length] sectors. If the
      active flag is set then the partition will be marked
      as active/bootable. If partition type [ty] is given then
      this will determine the advertised filesystem type, by default
      this is set to 6 (FAT16) *)

  val unmarshal: Cstruct.t -> (t, string) result
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

val make: Partition.t list -> t
(** [make partitions] constructs an MBR given a desired list of
    primary partitions *)

val marshal: Cstruct.t -> t -> unit

val unmarshal: Cstruct.t -> (t, string) result

val sizeof: int

val default_partition_start: int32
(** default sector offset for first partition *)

(* {1} Dynamically-typed query interface *)

val all: string list

val get: t -> string -> string option
