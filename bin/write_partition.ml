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

let copy ic oc max_bytes =
  let buf_len = 4096 in
  let buf = Bytes.create buf_len in
  let rec loop i =
    let len = input ic buf 0 buf_len in
    if len > 0 then
      let len' = min len (max_bytes - i) in
      output oc buf 0 len';
      if i + len > max_bytes then
        failwith "Trying to write more than can fit in partition";
      loop (i + len')
    else
      ()
  in
  loop 0

let write_to_partition mbr partition_number input_data =
  let partition = get_partition_info mbr partition_number in
  let start_sector, num_sectors, sector_size =
    calculate_partition_info partition
  in
  if start_sector = 0 then
    Printf.ksprintf failwith "Writing to partition %d would overwrite the MBR header" partition_number;
  let ic, data_size =
    match input_data with
    | None ->
        (stdin, None)
    | Some file_path ->
        let file_info = Unix.stat file_path in
        let data_size = file_info.st_size in
        let ic = open_in_bin file_path in
        (ic, Some data_size)
  in
  let partition_size = num_sectors * sector_size in
  Option.iter (fun data_size ->
      Printf.printf "Total input size: %d\n" data_size)
    data_size;
  Printf.printf "Total Partition size: %d\n" partition_size;
  Option.iter (fun data_size ->
      if data_size > partition_size then failwith "Input is too large for partition")
    data_size;
  Printf.printf "\nBegin writing to partition:- \n";
  let oc = open_out_gen [ Open_wronly; Open_binary ] 0o644 mbr in
  seek_out oc (start_sector * sector_size);
  copy ic oc partition_size;
  close_out_noerr oc;
  close_in_noerr ic

let mbr =
  let doc = "The disk image containing the partition" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"disk_image" ~doc)

let partition_number =
  let doc = "The partition number to write to" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"partition_number" ~doc)

let input_data =
  let doc = "The data to write to the partition." in
  Arg.(value & opt (some string) None & info [ "d"; "data" ] ~doc ~docv:"FILE")

let cmd =
  let doc = "Write data into a partition" in
  let info = Cmd.info "write_partition" ~version:"1.0.0" ~doc in
  Cmd.v info
    Term.(const write_to_partition $ mbr $ partition_number $ input_data)

let main () = exit (Cmd.eval cmd)
let () = main ()
