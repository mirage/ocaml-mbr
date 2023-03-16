open Cmdliner

let print_mbr_fields print_bootstrap_code mbr =
  Printf.printf "MBR fields:\n";
  if print_bootstrap_code then
    Printf.printf "  bootstrap_code: %s\n"
      (Cstruct.to_hex_string (Cstruct.of_string mbr.Mbr.bootstrap_code));
  Printf.printf "  original_physical_drive: %d\n"
    mbr.Mbr.original_physical_drive;
  Printf.printf "  seconds: %d\n" mbr.Mbr.seconds;
  Printf.printf "  minutes: %d\n" mbr.Mbr.minutes;
  Printf.printf "  hours: %d\n" mbr.Mbr.hours;
  Printf.printf "  disk_signature: %lx\n" mbr.Mbr.disk_signature;
  List.iteri
    (fun i part ->
      let chs_begin = part.Mbr.Partition.first_absolute_sector_chs in
      let chs_end = part.Mbr.Partition.last_absolute_sector_chs in
      Printf.printf "  Partition %d:\n" (i + 1);
      Printf.printf "    bootable: %b\n" part.Mbr.Partition.active;
      let { Mbr.Geometry.cylinders; Mbr.Geometry.heads; Mbr.Geometry.sectors } =
        chs_begin
      in
      Printf.printf "    chs_begin: (cylinders: %d, heads: %d, sectors: %d)\n"
        cylinders heads sectors;
      Printf.printf "    ty: %02x\n" part.Mbr.Partition.ty;
      let { Mbr.Geometry.cylinders; Mbr.Geometry.heads; Mbr.Geometry.sectors } =
        chs_end
      in
      Printf.printf "    chs_end: (cylinders: %d, heads: %d, sectors: %d)\n"
        cylinders heads sectors;
      Printf.printf "    lba_begin: %ld\n"
        part.Mbr.Partition.first_absolute_sector_lba;
      Printf.printf "    size_sectors: %ld\n" part.Mbr.Partition.sectors)
    mbr.partitions

let read_mbrs print_bootstrap_code mbrs =
  List.iter
    (fun mbr ->
      let ic = open_in_bin mbr in
      let buf = Bytes.create Mbr.sizeof in
      let () = really_input ic buf 0 Mbr.sizeof in
      close_in ic;
      match Mbr.unmarshal (Cstruct.of_bytes buf) with
      | Ok mbr -> print_mbr_fields print_bootstrap_code mbr
      | Error msg ->
          Printf.printf "Failed to read MBR from %s: %s\n" mbr msg;
          exit 1)
    mbrs

let mbrs = Arg.(non_empty & pos_all file [] & info [] ~docv:"disk_images")

let print_bootstrap_code =
  let doc = "Print the bootstrap code of the disks images." in
  Arg.(value & flag & info [ "b"; "booststrap-code" ] ~doc)

let cmd =
  let doc =
    "Inspect the Master Boot Record (MBR) headers of one or more disk images."
  in
  let info = Cmd.info "mbr_inspect" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const read_mbrs $ print_bootstrap_code $ mbrs)

let main () = exit (Cmd.eval cmd)
let () = main ()
