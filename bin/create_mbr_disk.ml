open Cmdliner

let create_mbr_disk destination partition_files =
  let sector_size = Mbr.sizeof in
  let num_partitions = List.length partition_files in
  if num_partitions > 4 then (
    Printf.printf
      "Too many partition files. Limit number of files to 4 as MBR supports at \
       most 4 partitions";
    exit 2)
  else (
    Printf.printf "Total partitions to create: %d\n" num_partitions;
    let partition_sizes =
      List.map (fun file -> (Unix.stat file).Unix.st_size) partition_files
    in

    let partition_size = List.fold_left ( + ) 0 partition_sizes in
    let mbr_size = Mbr.sizeof in
    let total_size = partition_size + mbr_size in

    Printf.printf "Total disk size: %d bytes\n" total_size;

    let partitions =
      List.mapi
        (fun i size ->
          Printf.printf "Creating partition: %d" (i + 1);
          let start_sector = (i + 1) * sector_size in
          let num_sectors = (size + sector_size - 1) / sector_size in
          match
            Mbr.Partition.make ~active:false ~partition_type:1
              (Int32.of_int start_sector)
              (Int32.of_int num_sectors)
          with
          | Ok partition ->
              Printf.printf " - OK\n";
              partition
          | Error msg ->
              Printf.printf "Failed to create MBR: %s" msg;
              exit 1)
        partition_sizes
    in
    (* Mbr.make smart constructor checks for partition overlap, more than 1 active partitions and too many partitions *)
    let mbr =
      match Mbr.make partitions with
      | Ok mbr -> mbr
      | Error msg ->
          Printf.printf "Failed to create MBR: %s" msg;
          exit 1
    in
    let oc = open_out_bin destination in
    let mbr_buffer = Cstruct.create Mbr.sizeof in
    Mbr.marshal mbr_buffer mbr;
    output oc (Cstruct.to_bytes mbr_buffer) 0 Mbr.sizeof;
    close_out oc;
    Unix.truncate destination total_size)

let destination =
  let doc = "Output file for the MBR formatted disk image" in
  Arg.(
    required
    & opt (some string) None
    & info [ "d"; "destination" ] ~docv:"destination" ~doc)

let partition_files =
  let doc = "Data files to be written to the partitions" in
  Arg.(value & pos_all file [] & info [] ~docv:"partition_files" ~doc)

let cmd =
  let doc = "Create an MBR formatted disk image with an MBR header." in
  let info = Cmd.info "create_mbr_disk" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const create_mbr_disk $ destination $ partition_files)

let main () = exit (Cmd.eval cmd)
let () = main ()
