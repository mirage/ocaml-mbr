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
  let start_sector =
    Int32.to_int partition.Mbr.Partition.first_absolute_sector_lba
  in
  let num_sectors = Int32.to_int partition.Mbr.Partition.sectors in
  let sector_size = 512 in
  (start_sector, num_sectors, sector_size)

let read_line () =
  try
    let line = input_line stdin in
    line ^ "\n"
  with End_of_file -> ""

let read_file file_path =
  let ic = open_in_bin file_path in
  let buf = Buffer.create 1024 in
  try
    while true do
      let bytes = Bytes.create 1024 in
      let bytes_read = input ic bytes 0 1024 in
      if bytes_read = 0 then raise Exit
      else Buffer.add_subbytes buf bytes 0 bytes_read
    done;
    assert false (* Unreachable *)
  with
  | Exit -> Buffer.contents buf
  | exn ->
      close_in ic;
      raise exn

let write_partition_data_internal mbr start_sector output_buffer =
  let sector_size = 512 in
  let fd = open_out_gen [ Open_wronly; Open_creat; Open_binary ] 0o644 mbr in
  let mbr_size = read_mbr mbr |> snd in
  let pos = (start_sector * sector_size) + mbr_size in
  let () = seek_out fd pos in
  let () = output_bytes fd output_buffer in
  close_out fd
  
let write_to_partition mbr partition_number input_data =
  let partition = get_partition_info mbr partition_number in
  let start_sector, num_sectors, sector_size =
    calculate_partition_info partition
  in
  let buffer =
    match input_data with
    | None -> Bytes.of_string (read_line ())
    | Some file_path ->
        read_file file_path |> Bytes.of_string (* Convert buffer to bytes *)
  in
  let output_buffer = Bytes.create (num_sectors * sector_size) in
  let input_buffer_size = Bytes.length buffer in
  if input_buffer_size > num_sectors * sector_size then
    failwith "Input is too large for partition"
  else Bytes.blit buffer 0 output_buffer 0 input_buffer_size;
  write_partition_data_internal mbr start_sector output_buffer

let mbr =
  let doc = "The disk image containing the partition" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"disk_image" ~doc)

let partition_number =
  let doc = "The partition number to write to" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"partition_number" ~doc)

let input_data =
  let doc = "The data to write to the partition." in
  Arg.(value & opt (some string) None & info [ "d"; "data" ] ~doc)

let cmd =
  let doc = "Read the contents of a partition" in
  let info = Cmd.info "read_partition" ~version:"1.0.0" ~doc in
  Cmd.v info
    Term.(const write_to_partition $ mbr $ partition_number $ input_data)

let main () = exit (Cmd.eval cmd)
let () = main ()
