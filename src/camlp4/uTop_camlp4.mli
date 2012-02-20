(*
 * uTop_camlp4.mli
 * ---------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

val parse_toplevel_phrase : string -> bool -> Parsetree.toplevel_phrase UTop.result
  (** Toplevel phrase parser for utop using camlp4. *)

val parse_toplevel_phrase_camlp4 : string -> bool -> Camlp4.PreCast.Ast.str_item UTop.result
  (** Camlp4 toplevel phrase parser. Same as {!parse_toplevel_phrase}
      but the result is not converted to an OCaml ast. *)

val convert_camlp4_toplevel_phrase : Camlp4.PreCast.Ast.str_item -> Parsetree.toplevel_phrase
  (** Converts a camlp4 toplevel phrase into a standard OCaml toplevel
      phrase. *)
