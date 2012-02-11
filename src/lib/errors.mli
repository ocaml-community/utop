(* Must be the same as driver/errors.mli from ocaml sources. *)
open Format
val report_error : formatter -> exn -> unit
