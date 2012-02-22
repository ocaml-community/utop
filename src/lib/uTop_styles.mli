(*
 * uTop_styles.mli
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(** Styled loaded from ~/.utoprc *)

(** Type of utop styles. *)
type styles = {
  mutable style_keyword : LTerm_style.t;
  mutable style_symbol : LTerm_style.t;
  mutable style_ident : LTerm_style.t;
  mutable style_module : LTerm_style.t;
  mutable style_constant : LTerm_style.t;
  mutable style_char : LTerm_style.t;
  mutable style_string : LTerm_style.t;
  mutable style_quotation : LTerm_style.t;
  mutable style_comment : LTerm_style.t;
  mutable style_doc : LTerm_style.t;
  mutable style_blanks : LTerm_style.t;
  mutable style_error : LTerm_style.t;
  mutable style_directive : LTerm_style.t;
  mutable style_paren : LTerm_style.t;
  mutable style_font : string option;
  mutable style_foreground : LTerm_style.color option;
  mutable style_background : LTerm_style.color option;
  mutable style_cursor : LTerm_style.color option;
}

val styles : styles
  (** The styles in use. *)

val load : unit -> unit Lwt.t
  (** Load resources into [styles]. *)

val stylise : (UTop_token.location -> LTerm_style.t -> unit) -> (UTop_token.t * UTop_token.location) list -> unit
  (** [stylise apply tokens] calls [apply] on all token locations with
      the associated style. *)
