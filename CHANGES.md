v0.2 (2014-08-18)
* add Mbr_partition: V1_LWT.BLOCK, for easy access to partitions via
  the standard Mirage block interface.
* use a polymorphic variant result type [`Ok of 'a | `Error of 'b]
