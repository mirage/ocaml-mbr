open Cmdliner

(*to be implemented*)
let resize_partition _disk _partition_number _new_size =
  ()

let mbr =
  let doc = "The disk image containing the partition" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"disk_image" ~doc)

let partition_number =
  let doc = "The partition number to resize" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"partition_number" ~doc)

let new_size =
  let doc = "The new size of the partition" in
  Arg.(required & pos 2 (some int) None & info [] ~docv:"new_Size" ~doc)

let cmd =
  let doc = "Resize a partition" in
  let info = Cmd.info "resize_partition" ~version:"1.0.0" ~doc in
  Cmd.v info
    Term.(const resize_partition $ mbr $ partition_number $ new_size)

let main () = exit (Cmd.eval cmd)
let () = main ()