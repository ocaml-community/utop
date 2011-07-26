(*
 * uTop_token.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

type t =
  | Symbol
  | Lident
  | Uident
  | Constant
  | Char
  | String of bool
  | Quotation
  | Comment
  | Doc
  | Blanks
  | Error
