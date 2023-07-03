(*
 * uTop_styles.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open UTop_token

let return, (>>=) = Lwt.return, Lwt.(>>=)

module String_set = Set.Make (String)
module Default_paths = UTop_private.Default_paths

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
  let fn = Default_paths.config_file_name in
  Lwt.catch
    (fun () ->
      LTerm_resources.load fn >>= fun res ->
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
      (match String.lowercase_ascii (LTerm_resources.get "profile" res) with
         | "light" -> UTop.set_profile UTop.Light
         | "dark" -> UTop.set_profile UTop.Dark
         | "" -> ()
         | str -> raise (LTerm_resources.Error (Printf.sprintf "invalid profile %S" str)));
      UTop_private.error_style := styles.style_error;
      UTop_private.autoload := LTerm_resources.get_bool "autoload" res <> Some false;
      (match LTerm_resources.get "external-editor" res with
       | "" -> ()
       | s  -> UTop.set_external_editor s);
      return ())
    (function
    | Unix.Unix_error(Unix.ENOENT, _, _) ->
        return ()
    | Unix.Unix_error (error, func, _arg) ->
        Logs_lwt.err (fun m -> m "cannot load styles from %S: %s: %s" fn func (Unix.error_message error))
    | exn -> Lwt.fail exn)

let stylise_filter_layout stylise tokens =
  let aux acc = function
    | (Comment (Comment_reg, _), loc) ->
        stylise loc styles.style_comment;
        acc
    | (Comment (Comment_doc, _), loc) ->
        stylise loc styles.style_doc;
        acc
    | (Blanks, loc) ->
        stylise loc styles.style_blanks;
        acc
    | x -> x :: acc
  in
  List.rev (List.fold_left aux [] tokens)

let rec stylise_rec stylise tokens =
  match tokens with
    | [] ->
        ()
    | (Symbol _, loc) :: tokens ->
        stylise loc styles.style_symbol;
        stylise_rec stylise tokens
    | (Lident id, loc) :: tokens ->
        stylise loc
          (if String_set.mem id !UTop.keywords then
             styles.style_keyword
           else
             styles.style_ident);
        stylise_rec stylise tokens
    | (Uident id, loc) :: tokens when String_set.mem id !UTop.keywords ->
        stylise loc styles.style_keyword;
        stylise_rec stylise tokens
    | (Uident _id, loc1) :: (Symbol ".", loc2) :: tokens ->
        stylise loc1 styles.style_module;
        stylise loc2 styles.style_symbol;
        stylise_rec stylise tokens
    | (Uident _id, loc) :: tokens ->
        stylise loc styles.style_ident;
        stylise_rec stylise tokens
    | (Constant _, loc) :: tokens ->
        stylise loc styles.style_constant;
        stylise_rec stylise tokens
    | (Char, loc) :: tokens ->
        stylise loc styles.style_char;
        stylise_rec stylise tokens
    | (String _, loc) :: tokens ->
        stylise loc styles.style_string;
        stylise_rec stylise tokens
    | (Quotation (items, _), _) :: tokens ->
        stylise_quotation_items stylise items;
        stylise_rec stylise tokens
    | (Error, loc) :: tokens ->
        stylise loc styles.style_error;
        stylise_rec stylise tokens
    | ((Comment _ | Blanks), _) :: _ ->
        assert false

and stylise_quotation_items stylise items =
  match items with
    | [] ->
        ()
    | (Quot_data, loc) :: items ->
        stylise loc styles.style_quotation;
        stylise_quotation_items stylise items
    | (Quot_anti anti, _) :: items ->
        stylise anti.a_opening styles.style_symbol;
        (match anti.a_name with
           | None ->
               ()
           | Some (loc1, loc2) ->
               stylise loc1 styles.style_module;
               stylise loc2 styles.style_symbol);
        let tokens = stylise_filter_layout stylise anti.a_contents in
        stylise_rec stylise tokens;
        (match anti.a_closing with
           | None ->
               ()
           | Some loc ->
               stylise loc styles.style_symbol);
        stylise_quotation_items stylise items

let stylise stylise tokens =
  let tokens = stylise_filter_layout stylise tokens in
  match tokens with
    | (Symbol "#", loc) :: tokens -> begin
        stylise loc styles.style_directive;
        match tokens with
          | ((Lident id | Uident id), loc) :: tokens ->
              stylise loc
                (if String_set.mem id !UTop.keywords then
                   styles.style_keyword
                 else
                   styles.style_directive);
              stylise_rec stylise tokens
          | tokens ->
              stylise_rec stylise tokens
      end
    | tokens ->
        stylise_rec stylise tokens
