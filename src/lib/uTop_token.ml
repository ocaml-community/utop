(*
 * uTop_token.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(** Tokens.

    The type of tokens is semi-structured: parentheses construct and
    quotations are nested and others tokens are flat list. *)

(** Locations in the source string, which is encoded in UTF-8. *)
type location = {
  idx1 : int;
  (** Start position in unicode characters. *)
  idx2 : int;
  (** Stop position in unicode characters. *)
  ofs1 : int;
  (** Start position in bytes. *)
  ofs2 : int;
  (** Stop position in bytes. *)
}

type t =
  | Symbol of string
  | Lident of string
  | Uident of string
  | Constant of string
  | Char
  | String of int * bool
      (** [String (quote_size, terminated)]. *)
  | Comment of comment_kind * bool
      (** [Comment (kind, terminated)]. *)
  | Blanks
  | Error
  | Quotation of (quotation_item * location) list * bool
      (** [Quotation (items, terminated)]. *)

and comment_kind =
  | Comment_reg
      (** Regular comment. *)
  | Comment_doc
      (** Documentation comment. *)

and quotation_item =
  | Quot_data
  | Quot_anti of antiquotation

and antiquotation = {
  a_opening : location;
  (** Location of the opening [$]. *)
  a_closing : location option;
  (** Location of the closing [$]. *)
  a_name : (location * location) option;
  (** Location of the name and colon if any. *)
  a_contents : (t * location) list;
  (** Contents of the location. *)
}
