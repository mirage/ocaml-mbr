open OUnit

open Lwt

let (>>|=) x f =
  x >>= function
  | `Error err -> assert_failure (Fake_block.error_message err)
  | `Ok y -> f y

let assert_equal_str = assert_equal ~printer:(fun x -> x)

let mbr_partition () =
  Lwt_unix.run begin
    Fake_block.connect () >>|= fun disk ->
    Fake_block.get_info disk >>= fun disk_info ->

    Fake_block.write disk 0L [Cstruct.of_string "sector0"] >>|= fun () ->
    Fake_block.write disk 1L [Cstruct.of_string "sector1"] >>|= fun () ->
    Fake_block.write disk 2L [Cstruct.of_string "sector2"] >>|= fun () ->
    Fake_block.write disk 3L [Cstruct.of_string "sector3"] >>|= fun () ->

    let module Partition = Mbr_partition.Make(Fake_block) in
    Partition.connect {
      Partition.b = disk;
      start_sector = 1L;
      length_sectors = 2L;
    } >>|= fun part1 ->

    let buffer = Cstruct.create disk_info.Fake_block.sector_size in

    (* Check writes *)

    Partition.write part1 (-1L) [buffer] >>= function
    | `Ok () -> assert_failure "Out-of-range write allowed!";
    | `Error _ ->

    Partition.write part1 (1L) [Cstruct.of_string "DATA"] >>= function
    | `Error _ -> assert_failure "In-range write failed!";
    | `Ok () ->

    Partition.write part1 (2L) [buffer] >>= function
    | `Ok () -> assert_failure "Out-of-range write allowed!";
    | `Error _ ->

    (* Check reads *)

    Partition.read part1 (-1L) [buffer] >>= function
    | `Ok () -> assert_failure "Out-of-range read allowed!";
    | `Error _ ->

    Partition.read part1 (1L) [buffer] >>= function
    | `Error _ -> assert_failure "In-range read failed!";
    | `Ok () ->
        Cstruct.sub buffer 0 7 |> Cstruct.to_string |> assert_equal_str "DATAor2";

    Partition.read part1 (2L) [buffer] >>= function
    | `Ok () -> assert_failure "Out-of-range read allowed!";
    | `Error _ ->

    Partition.read part1 (1L) [Cstruct.create (disk_info.Fake_block.sector_size + 1)] >>= function
    | `Ok () -> assert_failure "Out-of-range read allowed!";
    | `Error msg -> assert_equal_str "read 1+2 out of range" (Fake_block.error_message msg);

    Partition.read part1 (Int64.max_int) [buffer] >>= function
    | `Ok () -> assert_failure "Out-of-range read allowed!";
    | `Error msg -> assert_equal_str "read 9223372036854775807+1 out of range" (Fake_block.error_message msg);

    Fake_block.read disk 0L [buffer] >>|= fun () -> Cstruct.sub buffer 0 7 |> Cstruct.to_string |> assert_equal_str "sector0";
    Fake_block.read disk 1L [buffer] >>|= fun () -> Cstruct.sub buffer 0 7 |> Cstruct.to_string |> assert_equal_str "sector1";
    Fake_block.read disk 2L [buffer] >>|= fun () -> Cstruct.sub buffer 0 7 |> Cstruct.to_string |> assert_equal_str "DATAor2";
    Fake_block.read disk 3L [buffer] >>|= fun () -> Cstruct.sub buffer 0 7 |> Cstruct.to_string |> assert_equal_str "sector3";

    return ()
  end

(* Name the test cases and group them together *)
let suite = "mbr">::: [
  "mbr_partition">:: mbr_partition;
]

let is_error = function
  | RFailure _ | RError _ -> true
  | _ -> false

let () =
  let results = run_test_tt_main suite in
  if List.exists is_error results then exit 1
