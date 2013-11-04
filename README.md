ocaml-mbr
=========

A simple library for manipulating Master Boot Records. The
primary purposes of this library are:
  1. to create bootable disk images creating
     [mirage](http://www.openmirage.org/) kernels
  2. for mirage kernels to read the partition tables on
     attached disks

To do items
-----------

* It's easy to specify nonsense, for example overlapping
  partition entries. We could do with a way to represent a
  disk as a set of non-overlapping extents which could be
  nested (partition / LVM volume / filesystem / file)

