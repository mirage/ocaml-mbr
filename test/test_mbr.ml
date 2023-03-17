let ( let* ) = Result.bind

let get_ok = function
  | Ok x -> x
  | Error s -> Alcotest.failf "expected Ok, got Error \"%S\"" s

module Testable_partition = struct
  let pp_geometry ppf { Mbr.Geometry.cylinders; heads; sectors } =
    Fmt.pf ppf "{ cylinders = %d; heads = %d; sectors = %d }" cylinders heads
      sectors

  let pp ppf
      {
        Mbr.Partition.active;
        first_absolute_sector_chs;
        ty;
        last_absolute_sector_chs;
        first_absolute_sector_lba;
        sectors;
      } =
    Fmt.pf ppf
      "{ active = %b; first_absolute_sector_chs = %a; ty = %d; \
       last_absolute_sector_chs = %a; first_absolute_sector_lba = %lu; sectors \
       = %lu }"
      active pp_geometry first_absolute_sector_chs ty pp_geometry
      last_absolute_sector_chs first_absolute_sector_lba sectors

  type t = Mbr.Partition.t

  let equal = ( = ) (* :/ *)
end

let partition =
  (module Testable_partition : Alcotest.TESTABLE with type t = Mbr.Partition.t)

let test_partition_make () =
  ignore
    (get_ok
       (Mbr.Partition.make ~partition_type:6 Mbr.default_partition_start 2048l))

let test_partition_make_ty_0 () =
  match Mbr.Partition.make ~partition_type:0 Mbr.default_partition_start 0l with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

let test_partition_make_ty_256 () =
  match
    Mbr.Partition.make ~partition_type:256 Mbr.default_partition_start 0l
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

let suite_partition_make =
  [
    ("Partition.make ok", `Quick, test_partition_make);
    ("Partition.make ~partition_type:0", `Quick, test_partition_make_ty_0);
    ("Partition.make ~partition_type:256", `Quick, test_partition_make_ty_256);
  ]

let test_make_empty () =
  match Mbr.make [] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "expected Ok, got %s" e

let test_make_too_many_partitions () =
  let r =
    let* p1 =
      Mbr.Partition.make ~partition_type:6 Mbr.default_partition_start 1l
    in
    let* p2 =
      Mbr.Partition.make ~partition_type:6
        (Int32.add Mbr.default_partition_start 1l)
        1l
    in
    let* p3 =
      Mbr.Partition.make ~partition_type:6
        (Int32.add Mbr.default_partition_start 2l)
        1l
    in
    let* p4 =
      Mbr.Partition.make ~partition_type:6
        (Int32.add Mbr.default_partition_start 3l)
        1l
    in
    let* p5 =
      Mbr.Partition.make ~partition_type:6
        (Int32.add Mbr.default_partition_start 4l)
        1l
    in
    Ok [ p1; p2; p3; p4; p5 ]
  in
  let ps = get_ok r in
  match Mbr.make ps with
  | Ok _ -> Alcotest.fail "expected Error"
  | Error _ -> ()

let test_make_overlapping () =
  let p1 = get_ok (Mbr.Partition.make ~partition_type:6 10l 10l) in
  let p2 = get_ok (Mbr.Partition.make ~partition_type:6 15l 10l) in
  match (Mbr.make [ p1; p2 ], Mbr.make [ p2; p1 ]) with
  | Ok _, _ | _, Ok _ -> Alcotest.fail "expected Error"
  | Error _, Error _ -> ()

let test_make_sorted () =
  let p1 = get_ok (Mbr.Partition.make ~partition_type:6 10l 1l) in
  let p2 = get_ok (Mbr.Partition.make ~partition_type:6 11l 1l) in
  let m1 = get_ok (Mbr.make [ p1; p2 ]) in
  let m2 = get_ok (Mbr.make [ p2; p1 ]) in
  (* polymorphic compare :'( *)
  Alcotest.(
    check (list partition) "partitions equal" m1.partitions m2.partitions)

let suite_make =
  [
    ("make []", `Quick, test_make_empty);
    ("make with 5 partitions", `Quick, test_make_too_many_partitions);
    ("make with overlapping partitions", `Quick, test_make_overlapping);
    ("make sorts partitions", `Quick, test_make_sorted);
  ]

let () =
  Alcotest.run "Mbr"
    [ ("Mbr.Partition.make", suite_partition_make); ("Mbr.make", suite_make) ]
