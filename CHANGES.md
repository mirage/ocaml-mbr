v0.4 (unreleased)
* Switch to dune
* Remove `Mbr_partition` and `Mbr_lwt`
* Remove old stringly typed interface
* Types are private
* Add helper functions to convert between uint32 MBR values and int64 values as expected in `Mirage_block`
* Update code and slim down on dependencies

v0.3 (2015-06-04)
* Expose a `connect` function for mirage-types > 2.3
* Fix bounds checks
* Add unit tests
* Fix integer overflow
* Add opam file

v0.2 (2014-08-18)
* add Mbr_partition: V1_LWT.BLOCK, for easy access to partitions via
  the standard Mirage block interface.
* use a polymorphic variant result type [`Ok of 'a | `Error of 'b]
