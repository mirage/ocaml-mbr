open Cmdliner

let mbr =
  let doc = "The disk image containing the partition" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"disk_image" ~doc)

let partition_number =
  let doc = "The partition number to write to" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"partition_number" ~doc)

let input_data =
  let doc = "The data to write to the partition." in
  Arg.(value & opt (some string) None & info [ "d"; "data" ] ~doc)

let cmd =
  let doc = "Read the contents of a partition" in
  let info = Cmd.info "read_partition" ~version:"1.0.0" ~doc in
  Cmd.v info
    Term.(const write_to_partition $ mbr $ partition_number $ input_data)

let main () = exit (Cmd.eval cmd)
let () = main ()
