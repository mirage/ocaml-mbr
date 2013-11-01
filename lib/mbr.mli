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

  val unmarshal: Cstruct.t -> (t, string) result
end

module Partition : sig
  type t = {
    active: bool;
    first_absolute_sector_chs: Geometry.t;
    ty: int;
    last_absolute_sector_chs: Geometry.t;
    first_absolute_sector_lba: int32;
    sectors: int32;
  }

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

val unmarshal: Cstruct.t -> (t, string) result

val sizeof: int

(* {1} Dynamically-typed query interface *)

val all: string list

val get: t -> string -> string option
