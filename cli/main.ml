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

let project_url = "http://github.com/djs55/ocaml-mbr"

open Common
open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [ 
 `S _common_options; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Options common to all commands *)
let common_options_t = 
  let docs = _common_options in 
  let debug = 
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in 
    Arg.(last & vflag_all [false] [verbose]) in 
  Term.(pure Common.make $ debug $ verb)

let filename =
  let doc = Printf.sprintf "Path to the file or device containing the mbr." in
  Arg.(value & pos 0 (some file) None & info [] ~doc)

let info_cmd =
  let doc = "display general information about a master boot record" in
  let man = [
    `S "DESCRIPTION";
    `P "Display general information about a master boot record.";
  ] @ help in
  Term.(ret(pure Impl.info $ common_options_t $ filename)),
  Term.info "info" ~sdocs:_common_options ~doc ~man

let write_cmd =
  let doc = "write an MBR with a single big partition" in
  let man = [
    `S "DESCRIPTION";
    `P "Write a simple MBR containing a single big partition.";
  ] @ help in
  Term.(ret(pure Impl.write $ common_options_t $ filename)),
  Term.info "write" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "manipulate master boot records" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "mbr-tool" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [info_cmd; write_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
