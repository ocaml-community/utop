(*
 * uTop_camlp5.mli
 * ---------------
 * Copyright : (c) 2012, Wojciech Meyer <wojciech.meyer@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

val parse_toplevel_phrase : string -> bool -> Parsetree.toplevel_phrase UTop.result
  (** Toplevel phrase parser for utop using camlp5. *)
val parse_toplevel_phrase_camlp5 : string -> bool -> MLast.str_item UTop.result
  (** Camlp5 toplevel phrase parser. Same as {!parse_toplevel_phrase}
      but the result is not converted to an OCaml ast. *)

val convert_camlp5_toplevel_phrase : MLast.str_item -> Parsetree.toplevel_phrase UTop.result
  (** Converts a camlp5 toplevel phrase into a standard OCaml toplevel
      phrase. Note that a camlp5 ast may not be convertible to an
      OCaml one, in which case it returns {!UTop.Error}. *)

val get_camlp5_error_message : exn -> UTop.location * string
  (** [get_camlp5_error_message exn] returns the location and error
      message for the exception [exn] as printed by camlp5. *)
