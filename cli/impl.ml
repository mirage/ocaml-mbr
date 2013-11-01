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

open Common
open Cmdliner
open Lwt

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let info common filename =
  try
    let filename = require "filename" filename in
    let mbr = Cstruct.create Mbr.sizeof in
    let t =
      Lwt_unix.openfile filename [ Lwt_unix.O_RDONLY ] 0o0 >>= fun fd ->
      Mbr_lwt.really_read fd mbr >>= fun () ->
      let mbr = match Mbr.unmarshal mbr with
      | Mbr.Error reason ->
        Printf.fprintf stderr "Failed to unmarshal MBR: %s\n%!" reason;
        exit 1
      | Mbr.Ok x -> x in
      let all = List.map (fun f ->
        match Mbr.get mbr f with
        | Some v -> [ f; v ]
        | None -> assert false
      ) Mbr.all in
      print_table ["field"; "value"] all;
      return () in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    Printf.fprintf stderr "Error: %s\n%!" x;
    exit 1

let write common filename =
  try
    let filename = require "filename" filename in
    let t =
      let mbr = Cstruct.create Mbr.sizeof in
      Lwt_unix.LargeFile.stat filename >>= fun st ->
      let total_bytes = st.Lwt_unix.LargeFile.st_size in
      let total_sectors = Int64.(to_int32 (div total_bytes 512L)) in
      let partition_length = Int32.sub total_sectors Mbr.default_partition_start in
      Mbr.marshal mbr (Mbr.make [Mbr.Partition.make ~active:true ~ty:6 Mbr.default_partition_start partition_length]);
      Lwt_unix.openfile filename [ Lwt_unix.O_WRONLY ] 0x0 >>= fun fd ->
      Mbr_lwt.really_write fd mbr >>= fun () ->
      Lwt_unix.close fd in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    Printf.fprintf stderr "Error: %s\n%!" x;
    exit 1
