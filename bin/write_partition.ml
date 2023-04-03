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

let read_multiline () =
  let rec read_lines acc =
    try
      let line = input_line stdin in
      if String.length line > 0 && line.[0] = '>' then
        String.concat "\n" (List.rev acc)
      else read_lines (line :: acc)
    with End_of_file -> String.concat "\n" (List.rev acc)
  in
  read_lines []

let read_file file_path chunk_size pos =
  Printf.printf "\nRead position in data file: %d\n" pos;
  let ic = open_in_bin file_path in
  try
    seek_in ic pos;
    let bytes = Bytes.create chunk_size in
    let bytes_read = input ic bytes 0 chunk_size in
    close_in ic;
    Bytes.sub_string bytes 0 bytes_read
  with exn ->
    close_in ic;
    raise exn

let write_partition_data_internal fd start_sector write_sector output_buffer =
  Printf.printf "Position written on disk: %d\n" write_sector;
  let sector_size = 512 in
  let pos = (start_sector * sector_size) + write_sector in
  let () = seek_out fd pos in
  let () = output_bytes fd output_buffer in
  ()

let write_to_partition mbr partition_number input_data =
  let partition = get_partition_info mbr partition_number in
  let start_sector, num_sectors, sector_size =
    calculate_partition_info partition
  in
  let start_position = 0 in
  let data_chunk_size = 1024 in
  let buffer, data_size =
    match input_data with
    | None ->
        let input_string = Bytes.of_string (read_multiline ()) in
        (input_string, Bytes.length input_string)
    | Some file_path ->
        let file_info = Unix.stat file_path in
        let data_size = file_info.st_size in
        let file_contents =
          read_file file_path data_chunk_size start_position |> Bytes.of_string
        in
        (file_contents, data_size)
  in
  let partition_size = num_sectors * sector_size in
  Printf.printf "Total input size: %d\n" data_size;
  Printf.printf "Chunk size: %d\n" data_chunk_size;
  Printf.printf "Total Partition size: %d\n" partition_size;
  if data_size > partition_size then failwith "Input is too large for partition"
  else (
    Printf.printf "\nBegin writing to partition:- \n";
    let fd = open_out_gen [ Open_wronly; Open_binary ] 0o644 mbr in
    let rec write_chunks start_pos =
      if start_pos >= data_size then ()
      else
        let chunk_size = min data_chunk_size (data_size - start_pos) in
        if chunk_size <= 0 then ()
        else
          let chunk =
            match input_data with
            | None ->
                let chunk = Bytes.create chunk_size in
                Bytes.blit buffer start_pos chunk 0 chunk_size;
                chunk
            | Some file_path ->
                read_file file_path chunk_size start_pos |> Bytes.of_string
          in
          write_partition_data_internal fd start_sector start_pos chunk;
          write_chunks (start_pos + chunk_size)
    in
    write_chunks 0;
    close_out fd)

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
  let doc = "Write data into a partition" in
  let info = Cmd.info "write_partition" ~version:"1.0.0" ~doc in
  Cmd.v info
    Term.(const write_to_partition $ mbr $ partition_number $ input_data)

let main () = exit (Cmd.eval cmd)
let () = main ()
