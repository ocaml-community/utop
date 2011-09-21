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
   | UI                                                              |
   +-----------------------------------------------------------------+ *)

type ui = UTop_private.ui = Console | GTK | Emacs

let get_ui () = S.value UTop_private.ui

let exec_in_gui f = !UTop_private.exec_in_gui f

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
   | Hooks                                                           |
   +-----------------------------------------------------------------+ *)

let new_command_hooks = Lwt_sequence.create ()
let at_new_command f = ignore (Lwt_sequence.add_l f new_command_hooks)
let new_prompt_hooks = Lwt_sequence.create ()
let at_new_prompt f = ignore (Lwt_sequence.add_l f new_prompt_hooks)

(* +-----------------------------------------------------------------+
   | Prompts                                                         |
   +-----------------------------------------------------------------+ *)

type profile = Dark | Light

let profile, set_profile = S.create Dark

let smart_accept = ref true

let size = UTop_private.size

let key_sequence = UTop_private.key_sequence

let count = UTop_private.count

let time = ref 0.

let () = at_new_prompt (fun () -> time := Unix.time ())

let make_prompt ui profile count size key_sequence (recording, macro_count, macro_counter) =
  let tm = Unix.localtime !time in
  let color dark light =
    match profile with
      | Dark -> dark
      | Light -> light
  in
  match ui with
    | Emacs ->
        [||]
    | GTK ->
        eval [B_fg (color lcyan blue);
              S (Printf.sprintf "utop[%d]> " count)]
    | Console ->
        let bold = profile = Dark in
        let txta =
          if key_sequence = [] then
            eval [
              B_bold bold;
              B_fg (color lcyan blue);
              S "─( ";
              B_fg (color lmagenta magenta); S (Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec); E_fg;
              S " )─< ";
              B_fg (color lyellow yellow); S (Printf.sprintf "command %d" count); E_fg;
              S " >─";
            ]
          else
            eval [
              B_bold bold;
              B_fg (color lcyan blue);
              S "─( ";
              B_fg (color lmagenta magenta); S (Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec); E_fg;
              S " )─< ";
              B_fg (color lyellow yellow); S (Printf.sprintf "command %d" count); E_fg;
              S " >─[ ";
              B_fg (color lgreen green); S (String.concat " " (List.map LTerm_key.to_string_compact key_sequence)); E_fg;
              S " ]─";
            ]
        in
        let txtb =
          if recording then
            eval [
              B_bold bold;
              B_fg (color lcyan blue);
              S "{ ";
              B_fg (color lwhite black); S (Printf.sprintf "counter: %d" macro_counter); E_fg;
              S " }─[ ";
              B_fg (color lwhite black); S (Printf.sprintf "macro: %d" macro_count); E_fg;
              S " ]─";
            ]
          else
            eval [
              B_bold bold;
              B_fg (color lcyan blue);
              S "{ ";
              B_fg (color lwhite black); S (Printf.sprintf "counter: %d" macro_counter); E_fg;
              S " }─";
            ]
        in
        Array.append (
          if Array.length txta + Array.length txtb > size.cols then
            Array.sub (Array.append txta txtb) 0 size.cols
          else
            Array.concat [
              txta;
              Array.make
                (size.cols - Array.length txta - Array.length txtb)
                (UChar.of_int 0x2500, { none with foreground = Some (color lcyan blue); bold = Some bold });
              txtb;
            ]
        ) [|(UChar.of_char '#', { none with foreground = Some (color lgreen green) }); (UChar.of_char ' ', none)|]

let prompt = ref (
  S.l6 make_prompt
    UTop_private.ui
    profile
    count
    size
    key_sequence
    (S.l3 (fun x y z -> (x, y, z))
       (Zed_macro.recording LTerm_read_line.macro)
       (Zed_macro.count LTerm_read_line.macro)
       (Zed_macro.counter LTerm_read_line.macro))
)

let prompt_continue = ref (S.map (fun profile -> [|(UChar.of_char '>', { none with foreground = Some (if profile = Dark then lgreen else green) }); (UChar.of_char ' ', LTerm_style.none)|]) profile)
let prompt_comment = ref (S.map (fun profile -> [|(UChar.of_char '*', { none with foreground = Some (if profile = Dark then lgreen else green) }); (UChar.of_char ' ', LTerm_style.none)|]) profile)

(* +-----------------------------------------------------------------+
   | Help                                                            |
   +-----------------------------------------------------------------+ *)

module Bindings = Zed_input.Make (LTerm_key)
module Keys_map = Map.Make (struct type t = LTerm_key.t list let compare = compare end)

let () =
  Hashtbl.add Toploop.directive_table "utop_help"
    (Toploop.Directive_none
       (fun () ->
          print_endline "If colors look too bright, try: UTop.set_profile UTop.Light

You can use the following commands to get more help:

#utop_bindings : list all the current key bindings
#utop_macro : display the currently recorded macro

For a complete description of utop, look at the utop(1) manual page."));

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
          let macro = Zed_macro.contents LTerm_read_line.macro in
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

