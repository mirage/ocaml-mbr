let print_mbr_fields mbr =
  Printf.printf "MBR fields:\n";
  Printf.printf "  bootstrap_code: \"\"\n";
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

let read_mbr mbr =
  let ic = open_in_bin mbr in
  let buf = Bytes.create Mbr.sizeof in
  let () = really_input ic buf 0 Mbr.sizeof in
  close_in ic;
  match Mbr.unmarshal (Cstruct.of_bytes buf) with
  | Ok mbr -> mbr
  | Error msg ->
      Printf.printf "Failed to read MBR from %s: %s\n" mbr msg;
      exit 1

let () =
  let command = Filename.basename Sys.argv.(0) in
  if Array.length Sys.argv < 2 then (
    Printf.printf
      "Inspect MBR Headers: \n\
      \ Usage %s <file1> [<file2> ...]\n\
      \ You must pass in at least one MBR header\n"
      command;
    exit 1)
  else
    let mbr_files =
      Array.of_list
        (Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)))
    in
    Array.iter
      (fun mbr_file ->
        let mbr = read_mbr mbr_file in
        print_mbr_fields mbr)
      mbr_files
