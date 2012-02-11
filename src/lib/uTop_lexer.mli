(*
 * uTop_lexer.mli
 * --------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

val lex_string : ?camlp4 : bool -> string -> (UTop_token.t * int * int * string) list
  (** [lex_string str] returns all the tokens contained in [str]. It
      returns a list of [(token, start_index, stop_index,
      contents)]. Indexes are in unicode characters.

      If [camlp4] is [true] then quotations are parsed. *)
