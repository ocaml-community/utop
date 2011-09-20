(*
 * uTop_styles.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open Lwt
open UTop_token

module String_set = Set.Make (String)

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

let stylise stylise tokens =
  let rec loop tokens =
    match tokens with
      | [] ->
          ()
      | (token, start, stop, src) :: rest ->
          match token with
            | Symbol ->
                stylise start stop styles.style_symbol;
                loop rest
            | Lident ->
                stylise start stop
                  (if String_set.mem src !UTop.keywords then
                     styles.style_keyword
                   else
                     styles.style_ident);
                loop rest
            | Uident ->
                if String_set.mem src !UTop.keywords then begin
                  stylise start stop styles.style_keyword;
                  loop rest
                end else
                  loop_after_uident start stop rest
            | Constant ->
                stylise start stop styles.style_constant;
                loop rest
            | Char ->
                stylise start stop styles.style_char;
                loop rest
            | String _ ->
                stylise start stop styles.style_string;
                loop rest
            | Quotation _ ->
                stylise start stop styles.style_quotation;
                loop rest
            | Comment _ ->
                stylise start stop styles.style_comment;
                loop rest
            | Doc _ ->
                stylise start stop styles.style_doc;
                loop rest
            | Blanks ->
                stylise start stop styles.style_blanks;
                loop rest
            | Error ->
                stylise start stop styles.style_error;
                loop rest
  and loop_after_uident uid_start uid_stop tokens =
    match tokens with
      | [] ->
          ()
      | (token, start, stop, src) :: rest ->
          match token with
            | Symbol ->
                if src = "." then
                  stylise uid_start uid_stop styles.style_module
                else
                  stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_symbol;
                loop rest
            | Lident ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop
                  (if String_set.mem src !UTop.keywords then
                     styles.style_keyword
                   else
                     styles.style_ident);
                loop rest
            | Uident ->
                stylise uid_start uid_stop styles.style_ident;
                if String_set.mem src !UTop.keywords then begin
                  stylise start stop styles.style_keyword;
                  loop rest
                end else
                  loop_after_uident start stop rest
            | Constant ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_constant;
                loop rest
            | Char ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_char;
                loop rest
            | String _ ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_string;
                loop rest
            | Quotation _ ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_quotation;
                loop rest
            | Comment _ ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_comment;
                loop_after_uident uid_start uid_stop rest
            | Doc _ ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_doc;
                loop_after_uident uid_start uid_stop rest
            | Blanks ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_blanks;
                loop_after_uident uid_start uid_stop rest
            | Error ->
                stylise uid_start uid_stop styles.style_ident;
                stylise start stop styles.style_error;
                loop rest
  and loop_sharp tokens =
    match tokens with
      | [] ->
          ()
      | (token, start, stop, src) :: rest ->
          match token with
            | Symbol ->
                if src = "#" then begin
                  stylise start stop styles.style_directive;
                  loop_directive rest
                end else begin
                  stylise start stop styles.style_symbol;
                  loop rest
                end
            | Lident ->
                stylise start stop
                  (if String_set.mem src !UTop.keywords then
                     styles.style_keyword
                   else
                     styles.style_ident);
                loop rest
            | Uident ->
                if String_set.mem src !UTop.keywords then begin
                  stylise start stop styles.style_keyword;
                  loop rest
                end else
                  loop_after_uident start stop rest
            | Constant ->
                stylise start stop styles.style_constant;
                loop rest
            | Char ->
                stylise start stop styles.style_char;
                loop rest
            | String _ ->
                stylise start stop styles.style_string;
                loop rest
            | Quotation _ ->
                stylise start stop styles.style_quotation;
                loop rest
            | Comment _ ->
                stylise start stop styles.style_comment;
                loop_sharp rest
            | Doc _ ->
                stylise start stop styles.style_doc;
                loop_sharp rest
            | Blanks ->
                stylise start stop styles.style_blanks;
                loop_sharp rest
            | Error ->
                stylise start stop styles.style_error;
                loop rest
  and loop_directive tokens =
    match tokens with
      | [] ->
          ()
      | (token, start, stop, src) :: rest ->
          match token with
            | Symbol ->
                stylise start stop styles.style_symbol;
                loop rest
            | Lident ->
                stylise start stop
                  (if String_set.mem src !UTop.keywords then
                     styles.style_keyword
                   else
                     styles.style_directive);
                loop rest
            | Uident ->
                if String_set.mem src !UTop.keywords then begin
                  stylise start stop styles.style_keyword;
                  loop rest
                end else
                  loop_after_uident start stop rest
            | Constant ->
                stylise start stop styles.style_constant;
                loop rest
            | Char ->
                stylise start stop styles.style_char;
                loop rest
            | String _ ->
                stylise start stop styles.style_string;
                loop rest
            | Quotation _ ->
                stylise start stop styles.style_quotation;
                loop rest
            | Comment _ ->
                stylise start stop styles.style_comment;
                loop_directive rest
            | Doc _ ->
                stylise start stop styles.style_doc;
                loop_directive rest
            | Blanks ->
                stylise start stop styles.style_blanks;
                loop_directive rest
            | Error ->
                stylise start stop styles.style_error;
                loop rest
  in
  loop_sharp tokens
