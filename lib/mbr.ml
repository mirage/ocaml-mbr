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

(* "modern standard" MBR from wikipedia: *)
cstruct mbr {
  uint8_t bootstrap_code[218];
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
  uint8_t signature[2]  (* 0x55; 0xaa *)
} as little_endian

let _ = assert(sizeof_mbr = 512)

