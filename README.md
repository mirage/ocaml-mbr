ocaml-mbr
=========

A simple library for manipulating Master Boot Records. The
primary purposes of this library are:
  1. to create bootable disk images creating
     [mirage](http://www.openmirage.org/) kernels
  2. for mirage kernels to read the partition tables on
     attached disks

Usage
-----
Define a single partition as follows:
```
    let disk_length_bytes = Int32.(mul (mul 16l 1024l) 1024l) in
    let disk_length_sectors = Int32.(div disk_length_bytes 512l) in

    let start_sector = 2048l in
    let length_sectors = Int32.sub disk_length_sectors start_sector in
    let partition = Mbr.Partition.make ~active:true ~ty:6 start_sector length_sectors in
    let mbr = Mbr.make [ partition ] in
```
You can write the MBR to sector zero of a block device ```B``` as follows:
```
    B.connect id >>|= fun device ->
    let sector = Cstruct.create 512 in
    Mbr.marshal sector mbr;
    B.write device 0L [ sector ] >>|= fun () ->
```
You can then create a Partition block device as follows:
```
    let module Partition = Mbr_partition.Make(B) in
    Partition.connect {
      Partition.b = device;
      start_sector = Int64.of_int32 start_sector;
      length_sectors = Int64.of_int32 length_sectors;
    } >>= fun partition_or_error ->
```

To do items
-----------

* It's easy to specify nonsense, for example overlapping
  partition entries. We could do with a way to represent a
  disk as a set of non-overlapping extents which could be
  nested (partition / LVM volume / filesystem / file)

