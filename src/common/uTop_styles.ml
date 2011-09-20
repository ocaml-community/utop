(*
 * uTop_styles.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open Lwt

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

let styles = {
  style_keyword = LTerm_style.none;
  style_symbol = LTerm_style.none;
  style_ident = LTerm_style.none;
  style_module = LTerm_style.none;
  style_constant = LTerm_style.none;
  style_char = LTerm_style.none;
  style_string = LTerm_style.none;
  style_quotation = LTerm_style.none;
  style_comment = LTerm_style.none;
  style_doc = LTerm_style.none;
  style_blanks = LTerm_style.none;
  style_error = LTerm_style.none;
  style_directive = LTerm_style.none;
  style_paren = LTerm_style.none;
  style_font = None;
  style_foreground = None;
  style_background = None;
  style_cursor = None;
}

let load () =
  try_lwt
    lwt res = LTerm_resources.load (Filename.concat LTerm_resources.home ".utoprc") in
    styles.style_keyword <- LTerm_resources.get_style "keyword" res;
    styles.style_symbol <- LTerm_resources.get_style "symbol" res;
    styles.style_ident <- LTerm_resources.get_style "identifier" res;
    styles.style_module <- LTerm_resources.get_style "module" res;
    styles.style_constant <- LTerm_resources.get_style "constant" res;
    styles.style_char <- LTerm_resources.get_style "char" res;
    styles.style_string <- LTerm_resources.get_style "string" res;
    styles.style_quotation <- LTerm_resources.get_style "quotation" res;
    styles.style_comment <- LTerm_resources.get_style "comment" res;
    styles.style_doc <- LTerm_resources.get_style "doc" res;
    styles.style_blanks <- LTerm_resources.get_style "blanks" res;
    styles.style_error <- LTerm_resources.get_style "error" res;
    styles.style_directive <- LTerm_resources.get_style "directive" res;
    styles.style_paren <- LTerm_resources.get_style "parenthesis" res;
    styles.style_font <- (match LTerm_resources.get "font" res with
                            | "" -> None
                            | str -> Some str);
    styles.style_foreground <- LTerm_resources.get_color "foreground" res;
    styles.style_background <- LTerm_resources.get_color "background" res;
    styles.style_cursor <- LTerm_resources.get_color "cursor" res;
    (match String.lowercase (LTerm_resources.get "profile" res) with
       | "light" -> UTop.set_profile UTop.Light
       | "dark" -> UTop.set_profile UTop.Dark
       | "" -> ()
       | str -> raise (LTerm_resources.Error (Printf.sprintf "invalid profile %S" str)));
    return ()
  with Unix.Unix_error(Unix.ENOENT, _, _) ->
    return ()
