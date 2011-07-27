(*
 * uTop_token.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(** Type of tokens. Tokens with a boolean parameter takes as argument
    wheter the token is terminated or not. *)
type t =
  | Symbol
  | Lident
  | Uident
  | Constant
  | Char
  | String of bool
  | Quotation of bool
  | Comment of bool
  | Doc of bool
  | Blanks
  | Error
