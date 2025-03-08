ocaml-mbr
=========

A library for manipulating Master Boot Records. The
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
    B.connect id >>= fun device ->
    let sector = Cstruct.create 512 in
    Mbr.marshal sector mbr;
    B.write device 0L [ sector ] >>= fun () ->
    ...
```

To do items
-----------

* Implement tools to manipulate MBR-formatted disk images
  to construct, inspect or fill partitions that can later
  be used in Mirage unikernels.
  
