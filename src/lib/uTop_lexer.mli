(*
 * uTop_lexer.mli
 * --------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

val lex_string : string -> (UTop_token.t * UTop_token.location) list
  (** [lex_string str] returns all the tokens contained in
      [str]. *)
