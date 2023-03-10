let print_mbr_fields mbr =
  Printf.printf "MBR fields:\n";
  Printf.printf "  bootstrap_code: %s\n" mbr.Mbr.bootstrap_code;
  Printf.printf "  original_physical_drive: %d\n" mbr.Mbr.original_physical_drive;
  Printf.printf "  seconds: %d\n" mbr.Mbr.seconds;
  Printf.printf "  minutes: %d\n" mbr.Mbr.minutes;
  Printf.printf "  hours: %d\n" mbr.Mbr.hours;
  Printf.printf "  disk_signature: %ld\n" mbr.Mbr.disk_signature;
  List.iteri (fun i part ->
    Printf.printf "  Partition %d:\n" (i + 1);
    Printf.printf "    bootable: %b\n" part.Mbr.Partition.active;
  (*   Printf.printf "    chs_begin: %d %d %d\n" part.Mbr.Partition.first_absolute_sector_chs; *)
    Printf.printf "    ty: %02x\n" part.Mbr.Partition.ty;
  (*  Printf.printf "    chs_end: %d %d %d\n" part.Mbr.Partition.last_absolute_sector_chs; *)
  (*  Printf.printf "    lba_begin: %ld\n" part.Mbr.Partition.first_absolute_sector_lba; *)
    Printf.printf "    size_sectors: %ld\n" part.Mbr.Partition.sectors;
  ) mbr.Mbr.partitions

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
  let mbr = read_mbr "test.img" in
  print_mbr_fields mbr