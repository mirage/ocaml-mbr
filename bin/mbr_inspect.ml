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
 

let create_mbr =
  let disk_length_bytes = Int32.(mul (mul 16l 1024l) 1024l) in
  let disk_length_sectors = Int32.(div disk_length_bytes 512l) in

  let start_sector = 2048l in
  let length_sectors = Int32.sub disk_length_sectors start_sector in

  let partition1 =
    match Mbr.Partition.make ~active:true ~ty:0x07 start_sector length_sectors with
    | Ok part -> part
    | Error msg ->
        Printf.printf "Partition failed: %s\n" msg;
        exit 1
  in

  let mbr_create = Mbr.make [ partition1 ] in
  match mbr_create with
  | Ok mbr -> mbr;
  | Error msg ->
      Printf.printf "MBR failed: %s\n" msg;
      exit 1
 
let () =
  let mbr = create_mbr in
  print_mbr_fields mbr