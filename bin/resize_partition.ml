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

let get_partition_info mbr partition_num =
  let mbr = read_mbr mbr |> fst in
  List.nth mbr.Mbr.partitions (partition_num - 1)

let calculate_partition_info partition =
  (* FIXME: Use Int32.unsigned_to_int *)
  let start_sector =
    Int32.to_int partition.Mbr.Partition.first_absolute_sector_lba
  in
  let num_sectors = Int32.to_int partition.Mbr.Partition.sectors in
  let sector_size = 512 in
  (start_sector, num_sectors, sector_size)

(*to be implemented*)
let resize_partition disk partition_number _new_size =
  let partition = get_partition_info disk partition_number in
  let _start_sector, _num_sectors, _sector_size =
    calculate_partition_info partition
  in
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