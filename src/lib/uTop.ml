(*
 * uTop.ml
 * -------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

[@@@warning "-27"]

open CamomileLibraryDefault.Camomile
open Lwt_react
open LTerm_text
open LTerm_geom
open LTerm_style

module M = Directories.Base_dirs ()

let (>>=) = Lwt.(>>=)

let toploop_get_directive name =
#if OCAML_VERSION >= (4, 13, 0)
  Toploop.get_directive name
#else
  try Some (Hashtbl.find Toploop.directive_table name) with Not_found -> None
#endif

module String_set = Set.Make(String)

let version = "%%VERSION%%"

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

let history = LTerm_history.create []
let unopt = function
        | None -> "None"
        | Some(v) -> v
let history_file_name = ref(Some(unopt(M.cache_dir) ^ "/.utop_history"))
let history_file_max_size = ref None
let history_file_max_entries = ref None
let stashable_session_history = UTop_history.create ()

(* +-----------------------------------------------------------------+
   | Hooks                                                           |
   +-----------------------------------------------------------------+ *)

let new_command_hooks = LTerm_dlist.create ()
let at_new_command f = ignore (LTerm_dlist.add_l f new_command_hooks)

(* +-----------------------------------------------------------------+
   | Config                                                          |
   +-----------------------------------------------------------------+ *)

type ui = UTop_private.ui = Console | Emacs

let get_ui () = S.value UTop_private.ui

type profile = Dark | Light

let profile, set_profile = S.create Dark
let set_profile p = set_profile p

let size = UTop_private.size

let key_sequence = UTop_private.key_sequence

let count = UTop_private.count

let time = ref (Unix.time ())

let () = at_new_command (fun () -> time := Unix.time ())

let make_variable ?eq x =
  let signal, set = S.create ?eq x in
  let set v = set v in
  (signal, (fun () -> S.value signal), set)

let hide_reserved, get_hide_reserved, set_hide_reserved = make_variable true
let create_implicits, get_create_implicits, set_create_implicits = make_variable false
let show_box, get_show_box, set_show_box = make_variable true
let phrase_terminator, get_phrase_terminator, set_phrase_terminator = make_variable ";;"
let auto_run_lwt, get_auto_run_lwt, set_auto_run_lwt = make_variable true
let auto_run_async, get_auto_run_async, set_auto_run_async = make_variable true
let topfind_verbose, get_topfind_verbose, set_topfind_verbose = make_variable false
let external_editor, get_external_editor, set_external_editor =
  make_variable
    (try
       Sys.getenv "EDITOR"
     with Not_found ->
       "vi")

(* Ugly hack until the action system of lambda-term is improved *)
let end_and_accept_current_phrase : LTerm_read_line.action =
  Edit (Custom (fun () -> assert false))

let set_margin_function f = UTop_private.set_margin_function f

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

let keywords = ref (String_set.of_list default_keywords)
let add_keyword kwd = keywords := String_set.add kwd !keywords

(* +-----------------------------------------------------------------+
   | Span of Lines                                                   |
   +-----------------------------------------------------------------+ *)

type lines = {
  start: int;
  stop:  int;
}

(* +-----------------------------------------------------------------+
   | Error reporting                                                 |
   +-----------------------------------------------------------------+ *)

let get_message func x =
  let buffer = Buffer.create 1024 in
  let pp = Format.formatter_of_buffer buffer in
  UTop_private.set_margin pp;
  func pp x;
  Format.pp_print_flush pp ();
  Buffer.contents buffer

let get_ocaml_error_message exn =
  let buffer = Buffer.create 1024 in
  let pp = Format.formatter_of_buffer buffer in
  UTop_private.set_margin pp;
  Errors.report_error pp exn;
  Format.pp_print_flush pp ();
  let str = Buffer.contents buffer in
  try
    Scanf.sscanf
      str
      "Characters %d-%d:\n%[\000-\255]"
      (fun start stop msg -> ((start, stop), msg, None))
  with Scanf.Scan_failure(_) ->
  try
    Scanf.sscanf
      str
      "Line %d, characters %d-%d:\n%[\000-\255]"
      (fun line start stop msg -> ((start, stop), msg, Some{start=line; stop=line}))
  with Scanf.Scan_failure(_) ->
  try
    Scanf.sscanf
      str
      "Lines %d-%d, characters %d-%d:\n%[\000-\255]"
      (fun start_line stop_line start stop msg -> ((start, stop),
                                                   msg, Some{start=start_line;stop=stop_line}))
  with Scanf.Scan_failure(_) ->
    ((0, 0), str, None)

let collect_formatters buf pps f =
  (* First flush all formatters. *)
  List.iter (fun pp -> Format.pp_print_flush pp ()) pps;
  (* Save all formatter functions. *)
  let save = List.map (fun pp -> Format.pp_get_formatter_out_functions pp ()) pps in
  let restore () =
    List.iter2
      (fun pp out_functions ->
         Format.pp_print_flush pp ();
         Format.pp_set_formatter_out_functions pp out_functions)
      pps save
  in
  (* Output functions. *)
  let out_functions =
    let ppb = Format.formatter_of_buffer buf in
    Format.pp_get_formatter_out_functions ppb ()
  in
  (* Replace formatter functions. *)
  List.iter
    (fun pp ->
       UTop_private.set_margin pp;
       Format.pp_set_formatter_out_functions pp out_functions)
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
  let save = List.map (fun pp -> Format.pp_get_formatter_out_functions pp ()) pps in
  let restore () =
    List.iter2
      (fun pp out_functions ->
         Format.pp_print_flush pp ();
         Format.pp_set_formatter_out_functions pp out_functions)
      pps save
  in
  (* Output functions. *)
  let out_functions = {
    Format.out_string = (fun _ _ _ -> ()); out_flush = ignore;
    out_newline = ignore; out_spaces = ignore
#if OCAML_VERSION >= (4, 06, 0)
      ; out_indent = ignore
#endif
  } in
  (* Replace formatter functions. *)
  List.iter (fun pp -> Format.pp_set_formatter_out_functions pp out_functions) pps;
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

let input_name = "//toplevel//"

let lexbuf_of_string eof str =
  let pos = ref 0 in
  let lexbuf =
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
  in
  Location.init lexbuf input_name;
  lexbuf

let mkloc loc =
  (loc.Location.loc_start.Lexing.pos_cnum,
   loc.Location.loc_end.Lexing.pos_cnum)

let parse_default parse str eos_is_error =
  let eof = ref false in
  let lexbuf = lexbuf_of_string eof str in
  try
    (* Try to parse the phrase. *)
    let phrase = parse lexbuf in
    Value phrase
  with
    | _ when !eof && not eos_is_error ->
        (* This is not an error, we just need more input. *)
        raise Need_more
    | End_of_file ->
        (* If the string is empty, do not report an error. *)
        raise Need_more
    | Lexer.Error (error, loc) ->
#if OCAML_VERSION >= (4, 08, 0)
        (match Location.error_of_exn (Lexer.Error (error, loc)) with
        | Some (`Ok error)->
          Error ([mkloc loc], get_message Location.print_report error)
        | _-> raise Need_more)
#else
        Error ([mkloc loc], get_message Lexer.report_error error)
#endif
    | Syntaxerr.Error error -> begin
      match error with
      | Syntaxerr.Unclosed (opening_loc, opening, closing_loc, closing) ->
        Error ([mkloc opening_loc; mkloc closing_loc],
               Printf.sprintf "Syntax error: '%s' expected, the highlighted '%s' might be unmatched" closing opening)
      | Syntaxerr.Applicative_path loc ->
        Error ([mkloc loc],
               "Syntax error: applicative paths of the form F(X).t are not supported when the option -no-app-funct is set.")
      | Syntaxerr.Other loc ->
        Error ([mkloc loc],
               "Syntax error")
      | Syntaxerr.Expecting (loc, nonterm) ->
        Error ([mkloc loc],
               Printf.sprintf "Syntax error: %s expected." nonterm)
      | Syntaxerr.Variable_in_scope (loc, var) ->
        Error ([mkloc loc],
               Printf.sprintf "In this scoped type, variable '%s is reserved for the local type %s." var var)
      | Syntaxerr.Not_expecting (loc, nonterm) ->
          Error ([mkloc loc],
                 Printf.sprintf "Syntax error: %s not expected" nonterm)
      | Syntaxerr.Ill_formed_ast (loc, s) ->
          Error ([mkloc loc],
                 Printf.sprintf "Error: broken invariant in parsetree: %s" s)
#if OCAML_VERSION >= (4, 03, 0)
      | Syntaxerr.Invalid_package_type (loc, s) ->
          Error ([mkloc loc],
                 Printf.sprintf "Invalid package type: %s" s)
#endif
    end
    | Syntaxerr.Escape_error | Parsing.Parse_error ->
        Error ([mkloc (Location.curr lexbuf)],
               "Syntax error")
    | exn ->
        Error ([], "Unknown parsing error (please report it to the utop project): " ^ Printexc.to_string exn)

let parse_toplevel_phrase_default = parse_default Parse.toplevel_phrase
let parse_toplevel_phrase = ref parse_toplevel_phrase_default

let parse_use_file_default = parse_default Parse.use_file
let parse_use_file = ref parse_use_file_default

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

let with_loc loc str = {
  Location.txt = str;
  Location.loc = loc;
}

#if OCAML_VERSION >= (4, 03, 0)
let nolabel = Asttypes.Nolabel
#else
let nolabel = ""
#endif

(* Check that the given phrase can be evaluated without typing/compile
   errors. *)
let check_phrase phrase =
  let open Parsetree in
  match phrase with
    | Ptop_dir _ ->
        None
    | Ptop_def [] ->
        None
    | Ptop_def (item :: items) ->
        let loc = {
          Location.loc_start = item.pstr_loc.Location.loc_start;
          Location.loc_end = (last item items).pstr_loc.Location.loc_end;
          Location.loc_ghost = false;
        } in
        (* Backup. *)
        let snap = Btype.snapshot () in
        let env = !Toploop.toplevel_env in
        (* Construct "let _ () = let module _ = struct <items> end in ()" in order to test
           the typing and compilation of [items] without evaluating them. *)
        let unit = with_loc loc (Longident.Lident "()") in
        let top_def =
          let open Ast_helper in
          with_default_loc loc
            (fun () ->
               Str.eval
                 (Exp.fun_ nolabel None (Pat.construct unit None)
                   (Exp.letmodule (with_loc loc
                        #if OCAML_VERSION >= (4, 10, 0)
                        (Some "_")
                        #else
                          "_"
                        #endif
                        )
                      (Mod.structure (item :: items))
                      (Exp.construct unit None))))
        in
        let check_phrase = Ptop_def [top_def] in
        try
          let _ =
            discard_formatters [Format.err_formatter] (fun () ->
              Env.reset_cache_toplevel ();
              Toploop.execute_phrase false null check_phrase)
          in
          (* The phrase is safe. *)
          Toploop.toplevel_env := env;
          Btype.backtrack snap;
          None
        with exn ->
          (* The phrase contains errors. *)
          let loc, msg, line = get_ocaml_error_message exn in
          Toploop.toplevel_env := env;
          Btype.backtrack snap;
          Some ([loc], msg, [line])

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
            S " # ";
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
                (Zed_char.unsafe_of_uChar (UChar.of_int 0x2500), { none with foreground = Some (color lcyan blue); bold = Some bold });
              txtb;
            ]
        ) second_line

let default_prompt =
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

let prompt = ref default_prompt

let edit_mode= ref LTerm_editor.Default

let default_info = {
  Toploop.section = "UTop";
  doc = ""; (* TODO: have some kind of documentation *)
}

let () =
  Toploop.add_directive "utop_prompt_simple"
    (Toploop.Directive_none
       (fun () ->
         prompt := S.map (Printf.ksprintf LTerm_text.of_utf8 "utop [%d]: ") count))
    default_info;

  Toploop.add_directive "utop_prompt_dummy"
    (Toploop.Directive_none
       (fun () ->
         prompt := S.const (LTerm_text.of_utf8 "# ")))
    default_info;

  Toploop.add_directive "utop_prompt_fancy_light"
    (Toploop.Directive_none
       (fun () ->
         set_profile Light;
         prompt := default_prompt))
    default_info;

  Toploop.add_directive "utop_prompt_fancy_dark"
    (Toploop.Directive_none
       (fun () ->
         set_profile Dark;
         prompt := default_prompt))
    default_info;

  Toploop.add_directive "edit_mode_default"
    (Toploop.Directive_none
       (fun () ->
         edit_mode:= LTerm_editor.Default))
    default_info;

  Toploop.add_directive "edit_mode_vi"
    (Toploop.Directive_none
       (fun () ->
         edit_mode:= LTerm_editor.Vi))
    default_info

(* +-----------------------------------------------------------------+
   | Help                                                            |
   +-----------------------------------------------------------------+ *)

module Bindings = Zed_input.Make (LTerm_key)
module Keys_map = Map.Make (struct type t = LTerm_key.t list let compare = compare end)

let name_of_action action =
  if action == end_and_accept_current_phrase then
    "end-and-accept-current-phrase"
  else
    LTerm_read_line.name_of_action action

let doc_of_action action =
  if action == end_and_accept_current_phrase then
    "end the current phrase with the phrase terminator (;;) and evaluate it"
  else
    LTerm_read_line.doc_of_action action

let () =
  Toploop.add_directive "utop_help"
    (Toploop.Directive_none
       (fun () ->
          print_endline "If you can't see the prompt properly try: #utop_prompt_simple

utop defines the following directives:

#help            : list all directives
#utop_bindings   : list all the current key bindings
#utop_macro      : display the currently recorded macro
#utop_stash      : store all the valid commands from your current session in a file
#utop_save       : store the current session with a simple prompt in a file
#topfind_log     : display messages recorded from findlib since the beginning of the session
#topfind_verbose : enable/disable topfind verbosity

For a complete description of utop, look at the utop(1) manual page."))
    default_info;

  Toploop.add_directive "utop_bindings"
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
                              name_of_action action,
                              doc_of_action action)
                             :: acc)
                  in
                  loop
                    actions
                    ((String.concat " " (List.map LTerm_key.to_string_compact keys),
                      name_of_action action,
                      doc_of_action action)
                     :: acc)
          in
          let bindings =
            Bindings.fold
              (fun key actions map ->
                 Keys_map.add key
                   (List.map (fun action -> (LTerm_read_line.Edit action)) actions) map)
              !LTerm_edit.bindings Keys_map.empty
          in
          let bindings = Bindings.fold Keys_map.add !LTerm_read_line.bindings bindings in
          let table =
            List.rev (Keys_map.fold (fun keys action acc -> make_lines keys action acc)
                        bindings [])
          in
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
          flush stdout))
    default_info;

  Toploop.add_directive "utop_macro"
    (Toploop.Directive_none
       (fun () ->
          let macro = Zed_macro.contents LTerm_read_line.macro in
          List.iter
            (fun action ->
               output_string stdout (name_of_action action);
               output_char stdout '\n')
            macro;
          flush stdout))
    default_info

let () =
  Toploop.add_directive "pwd"
    (Toploop.Directive_none
       (fun () -> print_endline (Sys.getcwd ())))
    default_info

let make_stash_directive entry_formatter fname =
  if get_ui () = Emacs then
    print_endline "Stashing is currently not supported in Emacs"
  else
  let entries = UTop_history.contents stashable_session_history in
  (* remove the stash directive from its output *)
  let entries = match entries with [] -> [] | _ :: e -> e in
  let entries = List.rev entries in
  Printf.printf "Stashing %d entries in %s ... " (List.length entries) fname;
  try
    let oc = open_out fname in
    try
      List.iter
        (fun e ->
           let line = entry_formatter e in
           output_string oc line;
           output_char oc '\n')
        entries;
      close_out oc;
      Printf.printf "Done.\n";
    with exn ->
      close_out oc;
      raise exn
  with exn ->
    Printf.printf "Error with file %s: %s\n" fname @@ Printexc.to_string exn

let () =
  let fn = make_stash_directive begin function
  | UTop_history.Input i ->
    i
  | Output out | Error out | Bad_input out | Warnings out ->
    Printf.sprintf "(* %s *)" out
  end
  in
  Toploop.add_directive "utop_stash" (Toploop.Directive_string fn) default_info

let () =
  let fn = make_stash_directive begin function
  | UTop_history.Input i | Bad_input i ->
    Printf.sprintf "# %s" i
  | Output out | Error out | Warnings out ->
    out
  end
  in
  Toploop.add_directive "utop_save" (Toploop.Directive_string fn) default_info

(* +-----------------------------------------------------------------+
   | Findlib stuff                                                   |
   +-----------------------------------------------------------------+ *)

let print_error msg =
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.set_style term !UTop_private.error_style >>= fun () ->
  Lwt_io.print msg >>= fun () ->
  LTerm.set_style term LTerm_style.none >>= fun () ->
  LTerm.flush term

let handle_findlib_error = function
  | Failure msg ->
      Lwt_main.run (print_error msg)
  | Fl_package_base.No_such_package(pkg, reason) ->
      Lwt_main.run (print_error (Printf.sprintf "No such package: %s%s\n" pkg (if reason <> "" then " - " ^ reason else "")))
  | Fl_package_base.Package_loop pkg ->
      Lwt_main.run (print_error (Printf.sprintf "Package requires itself: %s\n" pkg))
  | exn ->
      raise exn

let topfind_log, set_topfind_log = S.create ~eq:(fun _ _ -> false) []

let () =
  let real_log = !Topfind.log in
  Topfind.log := fun str ->
    set_topfind_log (str :: S.value topfind_log);
    if S.value topfind_verbose then real_log str

let () =
  Toploop.add_directive
    "topfind_log"
    (Toploop.Directive_none
       (fun () ->
         List.iter (fun str -> print_string str; print_char '\n')
           (S.value topfind_log);
         flush  stdout))
    default_info;

  Toploop.add_directive
    "topfind_verbose"
    (Toploop.Directive_bool set_topfind_verbose)
    default_info

let split_words str =
  let len = String.length str in
  let is_sep = function
    | ' ' | '\t' | '\r' | '\n' | ',' -> true
    | _ -> false
  in
  let rec skip acc i =
    if i = len then
      acc
    else
      if is_sep str.[i] then
        skip acc (i + 1)
      else
        extract acc i (i + 1)
  and extract acc i j =
    if j = len then
      (String.sub str i (j - i)) :: acc
    else
      if is_sep str.[j] then
        skip (String.sub str i (j - i) :: acc) (j + 1)
      else
        extract acc i (j + 1)
  in
  List.rev (skip [] 0)

let require packages =
  try
    let eff_packages = Findlib.package_deep_ancestors !Topfind.predicates packages in
    Topfind.load eff_packages
  with exn ->
    handle_findlib_error exn

let () =
  Toploop.add_directive
    "require"
    (Toploop.Directive_string
       (fun str -> require (split_words str)))
    default_info

(* +-----------------------------------------------------------------+
   | Backports                                                       |
   +-----------------------------------------------------------------+ *)


let try_finally ~always work=
#if OCAML_VERSION >= (4, 08, 0)
    Misc.try_finally ~always work
#else
    Misc.try_finally work always
#endif

let use_output command =
  let fn = Filename.temp_file "ocaml" "_toploop.ml" in
  try_finally ~always:(fun () ->
    try Sys.remove fn with Sys_error _ -> ())
    (fun () ->
       match
         Printf.ksprintf Sys.command "%s > %s"
           command
           (Filename.quote fn)
       with
       | 0 ->
         ignore (Toploop.use_file Format.std_formatter fn : bool)
       | n ->
         Format.printf "Command exited with code %d.@." n)

let () =
  let name = "use_output" in
  if toploop_get_directive name = None then
    Toploop.add_directive
      name
      (Toploop.Directive_string use_output)
      default_info

(* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ *)

let () =
  (* "utop" is an internal library so it is not passed as "-package"
     to "ocamlfind ocamlmktop". *)
  Topfind.don't_load_deeply ["utop"];
  Topfind.add_predicates ["byte"; "toploop"];
  (* Add findlib path so Topfind is available and it won't be
     initialized twice if the user does [#use "topfind"]. *)
  Topdirs.dir_directory (Findlib.package_directory "findlib");
  (* Make UTop accessible. *)
  Topdirs.dir_directory (Findlib.package_directory "utop")

 (* +-----------------------------------------------------------------+
   | Compiler-libs re-exports                                        |
   +-----------------------------------------------------------------+ *)

#if OCAML_VERSION >= (4, 08, 0)
let get_load_path ()= Load_path.get_paths ()
let set_load_path path= Load_path.init path
#else
let get_load_path ()= !Config.load_path
let set_load_path path= Config.load_path := path
#endif

(* +-----------------------------------------------------------------+
   | Deprecated                                                      |
   +-----------------------------------------------------------------+ *)

let smart_accept = ref true
let new_prompt_hooks = LTerm_dlist.create ()
let at_new_prompt f = ignore (LTerm_dlist.add_l f new_prompt_hooks)
let prompt_continue = ref (S.const [| |])
let prompt_comment = ref (S.const [| |])
