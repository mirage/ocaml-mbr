## Unreleased
* Add optional argument `?disk_signature` to `Mbr.make` (@Burnleydev1, review by @reynir, #19)
* Make the partition type a required argument to `Mbr.Partition.make` and rename it `~partition_type` (@AryanGodara, review by @reynir, #20)
* Add tools for inspecting and modifying MBR, and reading/writing data to partitions. The command line tools are not installed as part of the opam package. The tools are `bin/mbr_inspect.exe`, `bin/read_partition.exe`, `bin/resize_partition.exe` and `bin/write_partition.exe`. (@PizieDust, review by @reynir, #22, #23, #24, #26)
* Remove dependency on `ppx_cstruct` (@reynir, #27)

## v1.0 (2022-09-27)
* Switch to dune
* Remove `Mbr_partition` and `Mbr_lwt`
* Remove old stringly typed interface
* Types are private
* Add helper functions to convert between uint32 MBR values and int64 values as expected in `Mirage_block`
* Update code and slim down on dependencies
* Handle empty partition entries

## v0.3 (2015-06-04)
* Expose a `connect` function for mirage-types > 2.3
* Fix bounds checks
* Add unit tests
* Fix integer overflow
* Add opam file

## v0.2 (2014-08-18)
* add `Mbr_partition: V1_LWT.BLOCK`, for easy access to partitions via
  the standard Mirage block interface.
* use a polymorphic variant result type `` [`Ok of 'a | `Error of 'b]``
