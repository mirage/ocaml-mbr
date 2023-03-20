open Cmdliner

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

let get_partition_info mbr partition_num =
  let mbr = read_mbr mbr in
  match partition_num with
  | 1 | 2 | 3 | 4 -> List.nth mbr.Mbr.partitions (partition_num - 1)
  | _ -> failwith "Partition number must be between 1 and 4"

let calculate_partition_info partition =
  let start_sector =
    Int32.to_int partition.Mbr.Partition.first_absolute_sector_lba
  in
  let num_sectors = Int32.to_int partition.Mbr.Partition.sectors in
  let sector_size = 512 in
  (start_sector, num_sectors, sector_size)

let read_partition_data mbr start_sector num_sectors sector_size =
  let buffer = Bytes.create (num_sectors * sector_size) in
  let ic = open_in_bin mbr in
  let offset = start_sector * sector_size in
  let () = seek_in ic offset in
  let () = really_input ic buffer 0 (num_sectors * sector_size) in
  close_in ic;
  buffer

let extract_partition_data mbr partition_num output_file =
  let partition = get_partition_info mbr partition_num in
  let start_sector, num_sectors, sector_size =
    calculate_partition_info partition
  in
  let buffer = read_partition_data mbr start_sector num_sectors sector_size in
  match output_file with
  | None -> print_string (Bytes.to_string buffer)
  | Some file_path ->
      let oc =
        open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o666 file_path
      in
      let () = output_bytes oc buffer in
      close_out oc

let mbr =
  let doc = "The disk image containing the partition" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"disk_image" ~doc)

let partition_number =
  let doc = "The partition number to read" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"partition_number" ~doc)

let output_to_file =
  let doc = "Output partition contents to a file" in
  Arg.(value & opt (some string) None & info [ "f"; "file" ] ~doc)

let cmd =
  let doc = "Read the contents of a partition" in
  let info = Cmd.info "read_partition" ~version:"1.0.0" ~doc in
  Cmd.v info
    Term.(
      const extract_partition_data $ mbr $ partition_number $ output_to_file)

let main () = exit (Cmd.eval cmd)
let () = main ()