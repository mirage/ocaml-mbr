let () =
  let command = Sys.argv.(0) in
  if Array.length Sys.argv < 2 then (
    Printf.printf
      "Inspect MBR Headers: \n\
      \ Usage %s <file1> [<file2> ...]\n\
      \ You must pass in at least one MBR header\n"
      command;
    exit 1)
  else
    let mbr_files =
      Array.of_list
        (Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)))
    in
    Array.iter
      (fun mbr_file ->
        let mbr = Mbr.read_mbr mbr_file in
        Mbr.print_mbr_fields mbr)
      mbr_files
