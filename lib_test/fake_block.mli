include V1_LWT.BLOCK with
   type id = unit

val error_message : error -> string
val connect : unit -> [`Ok of t | `Error of error] io
