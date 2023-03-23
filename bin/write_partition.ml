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
