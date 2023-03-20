open Cmdliner

let mbr =
  let doc = "The disk image containing the partition" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"disk_image" ~doc)

let partition_number =
  let doc = "The partition number to read" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"partition_number" ~doc)

let output_to_file =
  let doc = "Output partition contents to a file" in
  Arg.(value & opt (some string) None & info [ "f"; "file" ] ~doc)