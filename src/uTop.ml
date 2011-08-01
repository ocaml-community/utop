(*
 * uTop.ml
 * -------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open CamomileLibraryDyn.Camomile
open Lwt_react
open LTerm_text
open LTerm_geom
open LTerm_style

module String_set = Set.Make(String)

(* +-----------------------------------------------------------------+
   | Keywords                                                        |
   +-----------------------------------------------------------------+ *)

let default_keywords = [
  "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do";
  "done"; "downto"; "else"; "end"; "exception"; "external";
  "for"; "fun"; "function"; "functor"; "if"; "in"; "include";
  "inherit"; "initializer"; "lazy"; "let"; "match"; "method"; "module";
  "mutable"; "new"; "object";  "of";  "open"; "private";  "rec"; "sig";
  "struct";  "then";  "to";  "try";  "type";  "val"; "virtual";
  "when"; "while"; "with"; "try_lwt"; "finally"; "for_lwt"; "lwt";
]

let keywords = ref (List.fold_left (fun set kwd -> String_set.add kwd set) String_set.empty default_keywords)
let add_keyword kwd = keywords := String_set.add kwd !keywords

(* +-----------------------------------------------------------------+
   | Prompts                                                         |
   +-----------------------------------------------------------------+ *)

let size = UTop_private.size

let count = UTop_private.count

let make_prompt count size recording macro_count =
  let tm = Unix.localtime (Unix.time ()) in
  let txta =
    eval [
      B_bold true;
      B_fg lcyan;
      S "─( ";
      B_fg lmagenta; S (Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec); E_fg;
      S " )─< ";
      B_fg lyellow; S (Printf.sprintf "command %d" count); E_fg;
      S " >─";
    ]
  in
  let txtb =
    if recording then
      eval [
        B_bold true;
        B_fg lcyan;
        S "[ ";
        B_fg lwhite; S (Printf.sprintf "macro: %d" macro_count); E_fg;
        S " ]─";
      ]
    else
      [||]
  in
  Array.append (
    if Array.length txta + Array.length txtb > size.cols then
      Array.sub (Array.append txta txtb) 0 size.cols
    else
      Array.concat [
        txta;
        Array.make
          (size.cols - Array.length txta - Array.length txtb)
          (UChar.of_int 0x2500, { none with foreground = Some lcyan; bold = Some true });
        txtb;
      ]
  ) [|(UChar.of_char '#', { none with foreground = Some lgreen }); (UChar.of_char ' ', none)|]

let prompt = ref (S.l4 make_prompt count size (Zed_macro.recording LTerm_read_line.macro_recorder) (Zed_macro.count LTerm_read_line.macro_recorder))

let prompt_continue = ref (S.const [|(UChar.of_char '>', { none with foreground = Some lgreen }); (UChar.of_char ' ', LTerm_style.none)|])
let prompt_comment = ref (S.const [|(UChar.of_char '*', { none with foreground = Some lgreen }); (UChar.of_char ' ', LTerm_style.none)|])

(* +-----------------------------------------------------------------+
   | Help                                                            |
   +-----------------------------------------------------------------+ *)

module Bindings = Zed_input.Make (LTerm_key)
module Keys_map = Map.Make (struct type t = LTerm_key.t list let compare = compare end)

let () =
  Hashtbl.add Toploop.directive_table "utop_help"
    (Toploop.Directive_none
       (fun () ->
          print_endline "You can use the following commands to get more help:

#utop_bindings : list all the current key bindings
#utop_macro : display the currently recorded macro
"));

  Hashtbl.add Toploop.directive_table "utop_bindings"
    (Toploop.Directive_none
       (fun () ->
          let make_lines keys actions acc =
            match actions with
              | [] ->
                  (String.concat " " (List.map LTerm_key.to_string_compact keys),
                   "",
                   "does nothing")
                  :: acc
              | action :: actions ->
                  let rec loop actions acc =
                    match actions with
                      | [] ->
                          acc
                      | action :: actions ->
                          loop
                            actions
                            (("",
                              LTerm_read_line.name_of_action action,
                              LTerm_read_line.doc_of_action action)
                             :: acc)
                  in
                  loop
                    actions
                    ((String.concat " " (List.map LTerm_key.to_string_compact keys),
                      LTerm_read_line.name_of_action action,
                      LTerm_read_line.doc_of_action action)
                     :: acc)
          in
          let bindings = Bindings.fold (fun key actions map -> Keys_map.add key (List.map (fun action -> (LTerm_read_line.Edit action)) actions) map) !LTerm_edit.bindings Keys_map.empty in
          let bindings = Bindings.fold Keys_map.add !LTerm_read_line.bindings bindings in
          let table = List.rev (Keys_map.fold (fun keys action acc -> make_lines keys action acc) bindings []) in
          let size_key, size_name, size_doc =
            List.fold_left
              (fun (size_key, size_name, size_doc) (key, name, doc) ->
                 (max (String.length key) size_key,
                  max (String.length name) size_name,
                  max (String.length doc) size_doc))
              (0, 0, 0)
              table
          in
          let buf = Buffer.create 128 in
          let format_line (key, name, doc) =
            Buffer.clear buf;
            Buffer.add_string buf key;
            while Buffer.length buf < size_key do
              Buffer.add_char buf ' '
            done;
            Buffer.add_string buf " : ";
            Buffer.add_string buf name;
            while Buffer.length buf < size_key + size_name + 3 do
              Buffer.add_char buf ' '
            done;
            Buffer.add_string buf " -> ";
            Buffer.add_string buf doc;
            Buffer.add_char buf '\n';
            output_string stdout (Buffer.contents buf)
          in
          List.iter format_line table;
          flush stdout));

  Hashtbl.add Toploop.directive_table "utop_macro"
    (Toploop.Directive_none
       (fun () ->
          let macro = Zed_macro.contents LTerm_read_line.macro_recorder in
          List.iter
            (fun action ->
               output_string stdout (LTerm_read_line.name_of_action action);
               output_char stdout '\n')
            macro;
          flush stdout))

(* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ *)

let () =
  (* Do not load packages linked with the toplevel. *)
  Topfind.don't_load_deeply ["utop"; "findlib"; "lambda-term"]

