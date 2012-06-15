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

let version = UTop_version.version

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

let history = LTerm_history.create []
let history_file_name = ref (Some (Filename.concat LTerm_resources.home ".utop-history"))
let history_file_max_size = ref None
let history_file_max_entries = ref None

(* +-----------------------------------------------------------------+
   | Hooks                                                           |
   +-----------------------------------------------------------------+ *)

let new_command_hooks = Lwt_sequence.create ()
let at_new_command f = ignore (Lwt_sequence.add_l f new_command_hooks)

(* +-----------------------------------------------------------------+
   | Config                                                          |
   +-----------------------------------------------------------------+ *)

type ui = UTop_private.ui = Console | Emacs

let get_ui () = S.value UTop_private.ui

type profile = Dark | Light

let profile, set_profile = S.create Dark

let size = UTop_private.size

let key_sequence = UTop_private.key_sequence

let count = UTop_private.count

let time = ref 0.

let () = at_new_command (fun () -> time := Unix.time ())

let make_variable ?eq x =
  let signal, set = S.create ?eq x in
  (signal, (fun () -> S.value signal), set)

type syntax =
  | Normal
  | Camlp4o
  | Camlp4r

let syntax, get_syntax, set_syntax = make_variable Normal
let phrase_terminator, get_phrase_terminator, set_phrase_terminator = make_variable ";;"
let auto_run_lwt, get_auto_run_lwt, set_auto_run_lwt = make_variable true

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

let keywords = ref (List.fold_right String_set.add default_keywords String_set.empty)
let add_keyword kwd = keywords := String_set.add kwd !keywords

(* +-----------------------------------------------------------------+
   | Error reporting                                                 |
   +-----------------------------------------------------------------+ *)

let get_message func x =
  let buffer = Buffer.create 1024 in
  let pp = Format.formatter_of_buffer buffer in
  Format.pp_set_margin pp (S.value size).cols;
  func pp x;
  Format.pp_print_flush pp ();
  Buffer.contents buffer

let get_ocaml_error_message exn =
  let buffer = Buffer.create 1024 in
  let pp = Format.formatter_of_buffer buffer in
  Format.pp_set_margin pp (S.value size).cols;
  Errors.report_error pp exn;
  Format.pp_print_flush pp ();
  let str = Buffer.contents buffer in
  try
    Scanf.sscanf
      str
      "Characters %d-%d:\n%[\000-\255]"
      (fun start stop msg -> ((start, stop), msg))
  with _ ->
    ((0, 0), str)

let collect_formatters buf pps f =
  (* First flush all formatters. *)
  List.iter (fun pp -> Format.pp_print_flush pp ()) pps;
  (* Save all formatter functions. *)
  let save = List.map (fun pp -> Format.pp_get_all_formatter_output_functions pp ()) pps in
  let restore () =
    List.iter2
      (fun pp (out, flush, newline, spaces) ->
         Format.pp_print_flush pp ();
         Format.pp_set_all_formatter_output_functions pp ~out ~flush ~newline ~spaces)
      pps save
  in
  (* Output functions. *)
  let out str ofs len = Buffer.add_substring buf str ofs len in
  let flush = ignore in
  let newline () = Buffer.add_char buf '\n' in
  let spaces n = for i = 1 to n do Buffer.add_char buf ' ' done in
  (* Replace formatter functions. *)
  let cols = (S.value size).cols in
  List.iter
    (fun pp ->
       Format.pp_set_margin pp cols;
       Format.pp_set_all_formatter_output_functions pp ~out ~flush ~newline ~spaces)
    pps;
  try
    let x = f () in
    restore ();
    x
  with exn ->
    restore ();
    raise exn

let discard_formatters pps f =
  (* First flush all formatters. *)
  List.iter (fun pp -> Format.pp_print_flush pp ()) pps;
  (* Save all formatter functions. *)
  let save = List.map (fun pp -> Format.pp_get_all_formatter_output_functions pp ()) pps in
  let restore () =
    List.iter2
      (fun pp (out, flush, newline, spaces) ->
         Format.pp_print_flush pp ();
         Format.pp_set_all_formatter_output_functions pp ~out ~flush ~newline ~spaces)
      pps save
  in
  (* Output functions. *)
  let out str ofs len = () in
  let flush = ignore in
  let newline = ignore in
  let spaces = ignore in
  (* Replace formatter functions. *)
  List.iter (fun pp -> Format.pp_set_all_formatter_output_functions pp ~out ~flush ~newline ~spaces) pps;
  try
    let x = f () in
    restore ();
    x
  with exn ->
    restore ();
    raise exn

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

type location = int * int

type 'a result =
  | Value of 'a
  | Error of location list * string

exception Need_more

#if ocaml_version <= (3, 12, 1)
let input_name = ""
#else
let input_name = "//toplevel//"
#endif

let lexbuf_of_string eof str =
  let pos = ref 0 in
  Lexing.from_function
    (fun buf len ->
       if !pos = String.length str then begin
         eof := true;
         0
       end else begin
         let len = min len (String.length str - !pos) in
         String.blit str !pos buf 0 len;
         pos := !pos + len;
         len
       end)

let mkloc loc =
  (loc.Location.loc_start.Lexing.pos_cnum,
   loc.Location.loc_end.Lexing.pos_cnum)

let parse_toplevel_phrase_default str eos_is_error =
  let eof = ref false in
  let lexbuf = lexbuf_of_string eof str in
  try
    (* Try to parse the phrase. *)
    let phrase = Parse.toplevel_phrase lexbuf in
    Value phrase
  with
    | _ when !eof && not eos_is_error ->
        (* This is not an error, we just need more input. *)
        raise Need_more
    | End_of_file ->
        (* If the string is empty, do not report an error. *)
        raise Need_more
    | Lexer.Error (error, loc) ->
        Error ([mkloc loc], get_message Lexer.report_error error)
    | Syntaxerr.Error (Syntaxerr.Unclosed (opening_loc, opening, closing_loc, closing)) ->
        Error ([mkloc opening_loc; mkloc closing_loc],
               Printf.sprintf "Syntax error: '%s' expected, the highlighted '%s' might be unmatched" closing opening)
    | Syntaxerr.Error (Syntaxerr.Applicative_path loc) ->
        Error ([mkloc loc],
               "Syntax error: applicative paths of the form F(X).t are not supported when the option -no-app-funct is set.")
    | Syntaxerr.Error (Syntaxerr.Other loc) ->
        Error ([mkloc loc],
               "Syntax error")
    | Syntaxerr.Escape_error | Parsing.Parse_error ->
        Error ([mkloc (Location.curr lexbuf)],
               "Syntax error")
    | exn ->
        Error ([], "Unknown parsing error (please report it to the utop project): " ^ Printexc.to_string exn)

let parse_toplevel_phrase = ref parse_toplevel_phrase_default

(* +-----------------------------------------------------------------+
   | Safety checking                                                 |
   +-----------------------------------------------------------------+ *)

let null = Format.make_formatter (fun str ofs len -> ()) ignore

let rec last head tail =
  match tail with
    | [] ->
        head
    | head :: tail ->
        last head tail

#if ocaml_version >= (4, 0, 0)
let with_loc loc str = {
  Location.txt = str;
  Location.loc = loc;
}
#else
let with_loc loc str = str
#endif

(* Check that the given phrase can be evaluated without typing/compile
   errors. *)
let check_phrase phrase =
  match phrase with
    | Parsetree.Ptop_dir _ ->
        None
    | Parsetree.Ptop_def [] ->
        None
    | Parsetree.Ptop_def (item :: items) ->
        let loc = {
          Location.loc_start = item.Parsetree.pstr_loc.Location.loc_start;
          Location.loc_end = (last item items).Parsetree.pstr_loc.Location.loc_end;
          Location.loc_ghost = false;
        } in
        (* Backup. *)
        let snap = Btype.snapshot () in
        let env = !Toploop.toplevel_env in
        (* Construct "module _(_ : sig end) = struct <items> end" in
           order to test the typing and compilation of [items] without
           evaluating them. *)
        let wrapped_items = {
          Parsetree.pmod_loc = loc;
          Parsetree.pmod_desc = Parsetree.Pmod_structure (item :: items);
        } in
        let empty_sig = {
          Parsetree.pmty_loc = loc;
          Parsetree.pmty_desc = Parsetree.Pmty_signature [];
        } in
        let funct = {
          Parsetree.pmod_loc = loc;
          Parsetree.pmod_desc = Parsetree.Pmod_functor (with_loc loc "_", empty_sig, wrapped_items);
        } in
        let top_def = {
          Parsetree.pstr_loc = loc;
          Parsetree.pstr_desc = Parsetree.Pstr_module (with_loc loc "_", funct);
        } in
        let check_phrase = Parsetree.Ptop_def [top_def] in
        try
          let _ = discard_formatters [Format.err_formatter] (fun () -> Toploop.execute_phrase false null check_phrase) in
          (* The phrase is safe. *)
          Toploop.toplevel_env := env;
          Btype.backtrack snap;
          None
        with exn ->
          (* The phrase contains errors. *)
          Toploop.toplevel_env := env;
          Btype.backtrack snap;
          let loc, msg = get_ocaml_error_message exn in
          Some ([loc], msg)

(* +-----------------------------------------------------------------+
   | Prompt                                                          |
   +-----------------------------------------------------------------+ *)

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
        let second_line =
          eval [
            S "\n";
            B_bold bold;
            B_fg (rgb 0xe3 0xaa 0x73);
            S "utop";
            B_fg (color lgreen green);
            S " $ ";
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
        ) second_line

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
   | Camlp4                                                          |
   +-----------------------------------------------------------------+ *)

let print_error msg =
  lwt term = Lazy.force LTerm.stdout in
  lwt () = LTerm.set_style term !UTop_private.error_style in
  lwt () = Lwt_io.print msg in
  lwt () = LTerm.set_style term LTerm_style.none in
  LTerm.flush term

let handle_findlib_error = function
  | Failure msg ->
      Lwt_main.run (print_error msg)
  | Fl_package_base.No_such_package(pkg, reason) ->
      Lwt_main.run (print_error (Printf.sprintf "No such package: %s%S\n" pkg (if reason <> "" then " - " ^ reason else "")))
  | Fl_package_base.Package_loop pkg ->
      Lwt_main.run (print_error (Printf.sprintf "Package requires itself: %s\n" pkg))
  | exn ->
      raise exn

let set_syntax syntax =
  match get_syntax (), syntax with
    | Normal, Normal
    | Camlp4o, Camlp4o
    | Camlp4r, Camlp4r ->
        ()
    | (Camlp4o | Camlp4r), _ ->
        Lwt_main.run (print_error "Camlp4 already loaded, you cannot change the syntax now.\n")
    | Normal, Camlp4o -> begin
        set_syntax Camlp4o;
        set_phrase_terminator ";;";
        try
          Topfind.syntax "camlp4o";
          Topfind.load_deeply ["utop.camlp4"]
        with exn ->
          handle_findlib_error exn
      end
    | Normal, Camlp4r -> begin
        set_syntax Camlp4r;
        set_phrase_terminator ";";
        add_keyword "value";
        try
          Topfind.syntax "camlp4r";
          Topfind.load_deeply ["utop.camlp4"]
        with exn ->
          handle_findlib_error exn
      end

let () =
  Hashtbl.add
    Toploop.directive_table
    "camlp4o"
    (Toploop.Directive_none
       (fun () -> set_syntax Camlp4o));

  Hashtbl.add
    Toploop.directive_table
    "camlp4r"
    (Toploop.Directive_none
       (fun () -> set_syntax Camlp4r))

(* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ *)

let () =
  (* "utop" is an internal library so it is not passed as "-package"
     to "ocamlfind ocamlmktop". *)
  Topfind.don't_load ["utop"];
  (* Add findlib path so Topfind is available and it won't be
     initialized twice if the user does [#use "topfind"]. *)
  Topdirs.dir_directory (Findlib.package_directory "findlib");
  (* Make UTop accessible. *)
  Topdirs.dir_directory (Findlib.package_directory "utop")

(* +-----------------------------------------------------------------+
   | Deprecated                                                      |
   +-----------------------------------------------------------------+ *)

let smart_accept = ref true
let new_prompt_hooks = Lwt_sequence.create ()
let at_new_prompt f = ignore (Lwt_sequence.add_l f new_prompt_hooks)
let prompt_continue = ref (S.const [| |])
let prompt_comment = ref (S.const [| |])
