open Cmdliner

let read_mbr mbr =
  let ic = open_in_bin mbr in
  let buf = Bytes.create Mbr.sizeof in
  let () = really_input ic buf 0 Mbr.sizeof in
  close_in ic;
  match Mbr.unmarshal (Cstruct.of_bytes buf) with
  | Ok mbr -> (mbr, Mbr.sizeof)
  | Error msg ->
      Printf.printf "Failed to read MBR from %s: %s\n" mbr msg;
      exit 1

let get_partition_info mbr partition_number =
  List.nth mbr.Mbr.partitions (partition_number - 1)

let calculate_partition_info partition =
  (* FIXME: Use Int32.unsigned_to_int *)
  let start_sector =
    Int32.to_int partition.Mbr.Partition.first_absolute_sector_lba
  in
  let num_sectors = Int32.to_int partition.Mbr.Partition.sectors in
  let sector_size = 512 in
  Printf.printf "Current partition size: %d bytes\n" (num_sectors * sector_size);
  (start_sector, sector_size)

let make_new_partition partition start_sector sector_size new_size =
  if new_size mod sector_size <> 0 then
    Printf.ksprintf failwith
      "Partition cannot be resized. New size of %d bytes does not align to \
       sectors. New size must be a multiple of %d"
      new_size sector_size
  else
    let new_num_sectors = new_size / sector_size in
    let new_end_sector = start_sector + new_num_sectors in
    Printf.printf "New partition size: %d bytes\n"
      (new_num_sectors * sector_size);
    match
      Mbr.Partition.make ~active:partition.Mbr.Partition.active
        ~partition_type:partition.Mbr.Partition.ty
        partition.Mbr.Partition.first_absolute_sector_lba
        (Int32.of_int new_end_sector)
    with
    | Ok new_partition -> new_partition
    | Error msg -> failwith msg

let replace_partition_in_partition_table mbr partition_number new_partition =
  let update_partition i p =
    if i = partition_number - 1 then new_partition else p
  in
  List.mapi update_partition mbr.Mbr.partitions

(* Mbr.make smart constructor checks for partition overlap, more than 1 active partitions and too many partitions *)
let make_new_mbr mbr new_partition_table =
  match Mbr.make ~disk_signature:mbr.Mbr.disk_signature new_partition_table with
  | Ok new_mbr -> new_mbr
  | Error msg -> failwith msg

let resize_partition mbr partition_number new_size =
  let disk = mbr in
  let mbr = read_mbr mbr |> fst in
  let partition = get_partition_info mbr partition_number in
  let start_sector, sector_size = calculate_partition_info partition in
  let new_partition =
    make_new_partition partition start_sector sector_size new_size
  in
  let new_partition_table =
    replace_partition_in_partition_table mbr partition_number new_partition
  in
  let new_mbr = make_new_mbr mbr new_partition_table in
  let oc = open_out_gen [ Open_wronly; Open_binary ] 0o644 disk in
  seek_out oc 0;
  let buf = Cstruct.create Mbr.sizeof in
  Mbr.marshal buf new_mbr;
  let mbr_bytes = Cstruct.to_bytes buf in
  output oc mbr_bytes 0 Mbr.sizeof;
  close_out_noerr oc

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
  Cmd.v info Term.(const resize_partition $ mbr $ partition_number $ new_size)

let main () = exit (Cmd.eval cmd)
let () = main ()
