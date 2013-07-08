(*
 * uTop_main.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open CamomileLibraryDyn.Camomile
open Lwt
open Lwt_react
open LTerm_text
open LTerm_geom
open UTop_token
open UTop_styles
open UTop_private

module String_set = Set.Make(String)

exception Term of int

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

let save_history () =
  match !UTop.history_file_name with
    | None ->
        return ()
    | Some fn ->
        try_lwt
          LTerm_history.save UTop.history ?max_size:!UTop.history_file_max_size ?max_entries:!UTop.history_file_max_entries fn
        with Unix.Unix_error (error, func, arg) ->
          Lwt_log.error_f "cannot save history to %S: %s: %s" fn func (Unix.error_message error)

let init_history () =
  (* Save history on exit. *)
  Lwt_main.at_exit save_history;
  (* Load history. *)
  match !UTop.history_file_name with
    | None ->
        return ()
    | Some fn ->
        try_lwt
          LTerm_history.load UTop.history fn
        with Unix.Unix_error (error, func, arg) ->
          Lwt_log.error_f "cannot load history from %S: %s: %s" fn func (Unix.error_message error)

(* +-----------------------------------------------------------------+
   | offset --> index                                                |
   +-----------------------------------------------------------------+ *)

(* Return the index (in unicode characters) of the character starting
   a offset (in bytes) [ofs] in [str]. *)
let index_of_offset src ofs =
  let rec aux idx ofs' =
    if ofs' = ofs then
      idx
    else if ofs' > ofs then
      idx - 1
    else if ofs' = String.length src then
      -1
    else
      aux (idx + 1) (Zed_utf8.unsafe_next src ofs')
  in
  aux 0 0

let convert_locs str locs = List.map (fun (a, b) -> (index_of_offset str a, index_of_offset str b)) locs

(* +-----------------------------------------------------------------+
   | The read-line class                                             |
   +-----------------------------------------------------------------+ *)

let parse_input_multi input =
  let buf = Buffer.create 32 in
  let result =
    UTop.collect_formatters buf [Format.err_formatter]
      (fun () ->
         match !UTop.parse_use_file input false with
           | UTop.Error (locs, msg) ->
               UTop.Error (convert_locs input locs, "Error: " ^ msg ^ "\n")
           | UTop.Value phrases ->
               (UTop.Value phrases))
  in
  (result, Buffer.contents buf)

let parse_and_check input eos_is_error =
  let buf = Buffer.create 32 in
  let result =
    UTop.collect_formatters buf [Format.err_formatter]
      (fun () ->
         match !UTop.parse_toplevel_phrase input eos_is_error with
           | UTop.Error (locs, msg) ->
               UTop.Error (convert_locs input locs, "Error: " ^ msg ^ "\n")
           | UTop.Value phrase ->
               match UTop.check_phrase phrase with
                 | None ->
                     UTop.Value phrase
                 | Some (locs, msg) ->
                     UTop.Error (convert_locs input locs, msg))
  in
  (result, Buffer.contents buf)

(* Read a phrase. If the result is a value, it is guaranteed to by a
   valid phrase (i.e. typable and compilable). It also returns
   warnings printed parsing. *)
class read_phrase ~term = object(self)
  inherit [Parsetree.toplevel_phrase UTop.result * string] LTerm_read_line.engine ~history:(LTerm_history.contents UTop.history) () as super
  inherit [Parsetree.toplevel_phrase UTop.result * string] LTerm_read_line.term term as super_term

  val mutable return_value = None

  method eval =
    match return_value with
      | Some x ->
          x
      | None ->
          assert false

  method exec = function
    | LTerm_read_line.Accept :: actions when !UTop.smart_accept && S.value self#mode = LTerm_read_line.Edition -> begin
        Zed_macro.add self#macro LTerm_read_line.Accept;
        (* Try to parse the input. *)
        let input = Zed_rope.to_string (Zed_edit.text self#edit) in
        (* Toploop does that: *)
        Location.reset ();
        try
          let result = parse_and_check input false in
          return_value <- Some result;
          LTerm_history.add UTop.history input;
          return result
        with UTop.Need_more ->
          (* Input not finished, continue. *)
          self#insert (UChar.of_char '\n');
          self#exec actions
      end
    | actions ->
        super_term#exec actions

  method stylise last =
    let styled, position = super#stylise last in

    (* Syntax highlighting *)
    let stylise loc token_style =
      for i = loc.idx1 to loc.idx2 - 1 do
        let ch, style = styled.(i) in
        styled.(i) <- (ch, LTerm_style.merge token_style style)
      done
    in
    UTop_styles.stylise stylise (UTop_lexer.lex_string (UTop.get_syntax ()) (LTerm_text.to_string styled));

    if not last then
      (* Parenthesis matching. *)
      LTerm_text.stylise_parenthesis styled position styles.style_paren
    else begin
      match return_value with
        | Some (UTop.Error (locs, _), _) ->
            (* Highlight error locations. *)
            List.iter
              (fun (start, stop) ->
                 for i = start to stop - 1 do
                   let ch, style = styled.(i) in
                   styled.(i) <- (ch, { style with LTerm_style.underline = Some true })
                 done)
              locs
        | _ ->
            ()
    end;

    (styled, position)

  method completion =
    let pos, words =
      UTop_complete.complete
        ~syntax:(UTop.get_syntax ())
        ~phrase_terminator:(UTop.get_phrase_terminator ())
        ~input:(Zed_rope.to_string self#input_prev)
    in
    self#set_completion pos words

  method show_box = S.value self#mode <> LTerm_read_line.Edition || UTop.get_show_box ()

  initializer
    (* Set the source signal for the size of the terminal. *)
    UTop_private.set_size self#size;
    (* Set the source signal for the key sequence. *)
    UTop_private.set_key_sequence self#key_sequence;
    (* Set the prompt. *)
    self#set_prompt !UTop.prompt
end

(* +-----------------------------------------------------------------+
   | Out phrase printing                                             |
   +-----------------------------------------------------------------+ *)

let fix_string str =
  let len = String.length str in
  let ofs, _, _ = Zed_utf8.next_error str 0 in
  if ofs = len then
    str
  else begin
    let buf = Buffer.create (len + 128) in
    if ofs > 0 then Buffer.add_substring buf str 0 ofs;
    let rec loop ofs =
      Printf.bprintf buf "\\y%02x" (Char.code (String.unsafe_get str ofs));
      let ofs1 = ofs + 1 in
      let ofs2, _, _ = Zed_utf8.next_error str ofs1 in
      if ofs1 < ofs2 then
        Buffer.add_substring buf str ofs1 (ofs2 - ofs1);
      if ofs2 < len then
        loop ofs2
      else
        Buffer.contents buf
    in
    loop ofs
  end

let render_out_phrase term string =
  let string = fix_string string in
  let styled = LTerm_text.of_string string in
  let stylise loc token_style =
    for i = loc.idx1 to loc.idx2 - 1 do
      let ch, style = styled.(i) in
      styled.(i) <- (ch, LTerm_style.merge token_style style)
    done
  in
  UTop_styles.stylise stylise (UTop_lexer.lex_string (UTop.get_syntax ()) string);
  Lwt_main.run (LTerm.fprints term styled)

let orig_print_out_signature = !Toploop.print_out_signature
let orig_print_out_phrase = !Toploop.print_out_phrase

let rec map_items unwrap wrap items =
  match items with
  | [] ->
    []
  | item :: items ->
    let sig_item, _ = unwrap item in
    let name, rec_status =
      match sig_item with
      | Outcometree.Osig_class (_, name, _, _, rs)
      | Outcometree.Osig_class_type (_, name, _, _, rs)
      | Outcometree.Osig_module (name, _, rs)
      | Outcometree.Osig_type ((name, _, _, _, _), rs) ->
        (name, rs)
      | Outcometree.Osig_exception (name, _)
      | Outcometree.Osig_modtype (name, _)
      | Outcometree.Osig_value (name, _, _) ->
        (name, Outcometree.Orec_not)
    in
    let keep = name = "" || name.[0] <> '_' in
    if keep then
      item :: map_items unwrap wrap items
    else
      (* Replace the [Orec_next] at the head of items by [Orec_first] *)
      let items =
        match items with
        | [] ->
          []
        | item :: items' ->
          let sig_item, extra = unwrap item in
          match sig_item with
          | Outcometree.Osig_class (a, name, b, c, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_class (a, name, b, c, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_class_type (a, name, b, c, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_class_type (a, name, b, c, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_module (name, a, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_module (name, a, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_type ((name, a, b, c, d), rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_type ((name, a, b, c, d), Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_exception _
          | Outcometree.Osig_modtype _
          | Outcometree.Osig_value _ ->
            items
      in
      map_items unwrap wrap items

let print_out_signature pp items =
  if UTop.get_hide_reserved () then
    orig_print_out_signature pp (map_items (fun x -> (x, ())) (fun x () -> x) items)
  else
    orig_print_out_signature pp items

let print_out_phrase pp phrase =
  if UTop.get_hide_reserved () then
    let phrase =
      match phrase with
      | Outcometree.Ophr_eval _
      | Outcometree.Ophr_exception _ ->
        phrase
      | Outcometree.Ophr_signature items ->
        Outcometree.Ophr_signature (map_items (fun x -> x) (fun x y -> (x, y)) items)
    in
    orig_print_out_phrase pp phrase
  else
    orig_print_out_phrase pp phrase

let () =
  Toploop.print_out_signature := print_out_signature;
  Toploop.print_out_phrase := print_out_phrase

(* +-----------------------------------------------------------------+
   | Toplevel expression rewriting                                   |
   +-----------------------------------------------------------------+ *)

#if ocaml_version >= (4, 0, 0)
let with_loc loc str = {
  Location.txt = str;
  Location.loc = loc;
}
#else
let with_loc loc str = str
#endif

(* A rule for rewriting a toplevel expression. *)
type rewrite_rule = {
  required_values : Longident.t list;
  (* Values that must exist and be persistent for the rule to apply. *)
  rewrite : Location.t -> Parsetree.expression -> Parsetree.expression;
  (* The rewrite function. *)
  enabled : bool React.signal;
  (* Whether the rule is enabled or not. *)
}

(* Rewrite rules, indexed by the identifier of the type
   constructor. *)
let rewrite_rules : (Longident.t, rewrite_rule) Hashtbl.t = Hashtbl.create 42

let longident_lwt_main_run = Longident.Ldot (Longident.Lident "Lwt_main", "run")
let longident_async_core_thread_safe_block_on_async_exn =
  Longident.parse "Async.Std.Thread_safe.block_on_async_exn"
let longident_unit = Longident.Lident "()"

(* Wrap <expr> into: fun () -> <expr> *)
let wrap_unit loc e =
  let i = with_loc loc longident_unit in
  let p = {
    Parsetree.ppat_desc = Parsetree.Ppat_construct (i, None, false);
    Parsetree.ppat_loc = loc;
  } in
  {
    Parsetree.pexp_desc = Parsetree.Pexp_function ("", None, [(p, e)]);
    Parsetree.pexp_loc = loc;
  }

let () =
  (* Rewrite Lwt.t expressions to Lwt_main.run <expr> *)
  Hashtbl.add rewrite_rules (Longident.Ldot (Longident.Lident "Lwt", "t")) {
    required_values = [longident_lwt_main_run];
    rewrite = (fun loc e -> {
      Parsetree.pexp_desc =
        Parsetree.Pexp_apply
          ({ Parsetree.pexp_desc = Parsetree.Pexp_ident (with_loc loc longident_lwt_main_run);
             Parsetree.pexp_loc = loc },
           [("", e)]);
      Parsetree.pexp_loc = loc;
    });
    enabled = UTop.auto_run_lwt;
  };

  (* Rewrite Async.Std.Defered.t expressions to
     Async_core.Thread_safe.block_on_async_exn (fun () -> <expr>). *)
  Hashtbl.add rewrite_rules (Longident.parse "Async_core.Ivar.Deferred.t") {
    required_values = [longident_async_core_thread_safe_block_on_async_exn];
    rewrite = (fun loc e -> {
      Parsetree.pexp_desc =
        Parsetree.Pexp_apply
          ({ Parsetree.pexp_desc = Parsetree.Pexp_ident
              (with_loc loc longident_async_core_thread_safe_block_on_async_exn);
             Parsetree.pexp_loc = loc },
           [("", wrap_unit loc e)]);
      Parsetree.pexp_loc = loc;
    });
    enabled = UTop.auto_run_async;
  }

(* Returns whether the argument is a toplevel expression. *)
let is_eval = function
  | { Parsetree.pstr_desc = Parsetree.Pstr_eval _ } -> true
  | _ -> false

(* Returns whether the given path is persistent. *)
let rec is_persistent_path = function
  | Path.Pident id -> Ident.persistent id
  | Path.Pdot (p, _, _) -> is_persistent_path p
  | Path.Papply (_, p) -> is_persistent_path p

(* Convert a path to a long identifier. *)
let rec longident_of_path path =
  match path with
    | Path.Pident id ->
      Longident.Lident (Ident.name id)
    | Path.Pdot (path, s, _) ->
      Longident.Ldot (longident_of_path path, s)
    | Path.Papply (p1, p2) ->
      Longident.Lapply (longident_of_path p1, longident_of_path p2)

(* Returns the rewrite rule associated to a type, if any. *)
let rec rule_of_type typ =
  match typ.Types.desc with
    | Types.Tlink typ ->
      rule_of_type typ
    | Types.Tconstr (path, _, _) -> begin
      match try Some (Env.find_type path !Toploop.toplevel_env) with Not_found -> None with
      | Some {
        Types.type_kind = Types.Type_abstract;
        Types.type_private = Asttypes.Public;
        Types.type_manifest = Some typ;
      } ->
        rule_of_type typ
      | _ ->
        try
          Some (Hashtbl.find rewrite_rules (longident_of_path path))
        with Not_found ->
          None
    end
    | _ ->
      None

(* Check that the given long identifier is present in the environment
   and is persistent. *)
let is_persistent_in_env longident =
  try
    is_persistent_path (fst (Env.lookup_value longident !Toploop.toplevel_env))
  with Not_found ->
    false

#if ocaml_version >= (4, 0, 0)
let str_items_of_typed_structure tstr = tstr.Typedtree.str_items
let str_desc_of_typed_str_item tstr = tstr.Typedtree.str_desc
#else
let str_items_of_typed_structure tstr = tstr
let str_desc_of_typed_str_item tstr = tstr
#endif

let rewrite_str_item pstr_item tstr_item =
  match pstr_item, str_desc_of_typed_str_item tstr_item with
    | ({ Parsetree.pstr_desc = Parsetree.Pstr_eval e;
         Parsetree.pstr_loc = loc },
       Typedtree.Tstr_eval { Typedtree.exp_type = typ }) -> begin
      match rule_of_type typ with
        | Some rule ->
          if React.S.value rule.enabled && List.for_all is_persistent_in_env rule.required_values then
            { Parsetree.pstr_desc = Parsetree.Pstr_eval (rule.rewrite loc e);
              Parsetree.pstr_loc = loc }
          else
            pstr_item
        | None ->
          pstr_item
    end
    | _ ->
      pstr_item

let rewrite phrase =
  match phrase with
    | Parsetree.Ptop_def pstr ->
      if (UTop.get_auto_run_lwt () || UTop.get_auto_run_async ()) && List.exists is_eval pstr then
        let tstr, _, _ = Typemod.type_structure !Toploop.toplevel_env pstr Location.none in
        let tstr = str_items_of_typed_structure tstr in
        Parsetree.Ptop_def (List.map2 rewrite_str_item pstr tstr)
      else
        phrase
    | Parsetree.Ptop_dir _ ->
      phrase

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let rec read_phrase term =
  try_lwt
    (new read_phrase ~term)#run
  with Sys.Break ->
    lwt () = LTerm.fprintl term "Interrupted." in
    read_phrase term

let update_margin pp cols =
  if Format.pp_get_margin pp () <> cols then
    Format.pp_set_margin pp cols

let print_error term msg =
  lwt () = LTerm.set_style term styles.style_error in
  lwt () = Lwt_io.print msg in
  lwt () = LTerm.set_style term LTerm_style.none in
  LTerm.flush term

let rec loop term =
  (* Reset completion. *)
  UTop_complete.reset ();

  (* increment the command counter. *)
  UTop_private.set_count (S.value UTop_private.count + 1);

  (* Call hooks. *)
  Lwt_sequence.iter_l (fun f -> f ()) UTop.new_command_hooks;

  (* Read interactively user input. *)
  let phrase_opt =
    Lwt_main.run (
      try_lwt
        lwt result, warnings = read_phrase term in
        (* Print warnings before errors. *)
        lwt () = Lwt_io.print warnings in
        match result with
          | UTop.Value phrase ->
              return (Some phrase)
          | UTop.Error (_, msg) ->
              lwt () = print_error term msg in
              return None
      finally
        LTerm.flush term
    )
  in

  match phrase_opt with
    | Some phrase ->
        (* Rewrite toplevel expressions. *)
        let phrase = rewrite phrase in
        (* Set the margin of standard formatters. *)
        let cols = (LTerm.size term).cols in
        update_margin Format.std_formatter cols;
        update_margin Format.err_formatter cols;
        (* Formatter to get the output phrase. *)
        let buffer = Buffer.create 1024 in
        let pp = Format.formatter_of_buffer buffer in
        Format.pp_set_margin pp (LTerm.size term).cols;
        (try
#if ocaml_version > (4, 00, 1)
           Env.reset_cache_toplevel ();
#endif
           ignore (Toploop.execute_phrase true pp phrase);
           (* Flush everything. *)
           Format.pp_print_flush Format.std_formatter ();
           Format.pp_print_flush Format.err_formatter ();
           flush stdout;
           flush stderr;
           (* Get the string printed. *)
           Format.pp_print_flush pp ();
           let string = Buffer.contents buffer in
           match phrase with
             | Parsetree.Ptop_def _ ->
                 (* The string is an output phrase, colorize it. *)
                 render_out_phrase term string
             | Parsetree.Ptop_dir _ ->
                 (* The string is an error message. *)
                 Lwt_main.run (print_error term string)
         with exn ->
           (* The only possible errors are directive errors. *)
           let msg = UTop.get_message Errors.report_error exn in
           (* Skip the dumb location. *)
           let msg =
             try
               let idx = String.index msg '\n' + 1 in
               String.sub msg idx (String.length msg - idx)
             with Not_found ->
               msg
           in
           Lwt_main.run (print_error term msg));
        loop term
    | None ->
        loop term

(* +-----------------------------------------------------------------+
   | Welcome message                                                 |
   +-----------------------------------------------------------------+ *)

let welcome term =
  (* Create a context to render the welcome message. *)
  let size = LTerm.size term in
  let size = { rows = 3; cols = size.cols } in
  let matrix = LTerm_draw.make_matrix size in
  let ctx = LTerm_draw.context matrix size in

  (* Draw the message in a box. *)

  let message = Printf.sprintf "Welcome to utop version %s (using OCaml version %s)!" UTop.version Sys.ocaml_version in

  LTerm_draw.fill_style ctx LTerm_style.({ none with foreground = Some lcyan });

  LTerm_draw.draw_hline ctx 0 0 size.cols LTerm_draw.Light;
  LTerm_draw.draw_frame ctx {
    row1 = 0;
    row2 = 3;
    col1 = (size.cols - (String.length message + 4)) / 2;
    col2 = (size.cols + (String.length message + 4)) / 2;
  } LTerm_draw.Light;

  LTerm_draw.draw_styled ctx 1 ((size.cols - String.length message) / 2) (eval [B_fg LTerm_style.yellow; S message]);

  (* Render to the screen. *)
  lwt () = LTerm.print_box term matrix in

  (* Move to after the box. *)
  lwt () = LTerm.fprint term "\n" in

  LTerm.flush term

(* +-----------------------------------------------------------------+
   | Classic mode                                                    |
   +-----------------------------------------------------------------+ *)

let read_input_classic prompt buffer len =
  let rec loop i =
    if i = len then
      return (i, false)
    else
      Lwt_io.read_char_opt Lwt_io.stdin >>= function
        | Some c ->
            buffer.[i] <- c;
            if c = '\n' then
              return (i + 1, false)
            else
              loop (i + 1)
        | None ->
            return (i, true)
  in
  Lwt_main.run (Lwt_io.write Lwt_io.stdout prompt >> loop 0)

(* +-----------------------------------------------------------------+
   | Emacs mode                                                      |
   +-----------------------------------------------------------------+ *)

module Emacs(M : sig end) = struct

  (* Copy standard output, which will be used to send commands. *)
  let command_oc = Unix.out_channel_of_descr (Unix.dup Unix.stdout)

  let split_at ?(trim=false) ch str =
    let rec aux i j =
      if j = String.length str then
        if trim && i = j then
          []
        else
          [String.sub str i (j - i)]
      else if str.[j] = ch then
        String.sub str i (j - i) :: aux (j + 1) (j + 1)
      else
        aux i (j + 1)
    in
    aux 0 0

  (* +---------------------------------------------------------------+
     | Sending commands to Emacs                                     |
     +---------------------------------------------------------------+ *)

  (* Mutex used to send commands to Emacs. *)
  let command_mutex = Mutex.create ()

  let send command argument =
    Mutex.lock command_mutex;
    output_string command_oc command;
    output_char command_oc ':';
    output_string command_oc argument;
    output_char command_oc '\n';
    flush command_oc;
    Mutex.unlock command_mutex

  (* Keep the [utop-phrase-terminator] variable of the emacs part in sync. *)
  let () =
    S.keep (S.map (send "phrase-terminator") UTop.phrase_terminator)

  (* +---------------------------------------------------------------+
     | Standard outputs redirection                                  |
     +---------------------------------------------------------------+ *)

  (* The output of ocaml (stdout and stderr) is redirected so the
     emacs parts of utop can recognize it. *)

  (* Continuously copy the output of ocaml to Emacs. *)
  let rec copy_output which ic =
    let line = input_line ic in
    send which line;
    copy_output which ic

  (* Create a thread which redirect the given output: *)
  let redirect which fd =
    let fdr, fdw = Unix.pipe () in
    Unix.dup2 fdw fd;
    Unix.close fdw;
    Thread.create (copy_output which) (Unix.in_channel_of_descr fdr)

  (* Redirects stdout and stderr: *)
  let _ = redirect "stdout" Unix.stdout
  let _ = redirect "stderr" Unix.stderr

  (* +---------------------------------------------------------------+
     | Loop                                                          |
     +---------------------------------------------------------------+ *)

  let read_line () =
    let behavior = Sys.signal Sys.sigint Sys.Signal_ignore in
    try
      let line = Lwt_main.run (Lwt_io.read_line_opt Lwt_io.stdin) in
      Sys.set_signal Sys.sigint behavior;
      line
    with exn ->
      Sys.set_signal Sys.sigint behavior;
      raise exn

  let read_command () =
    match read_line () with
      | None ->
          None
      | Some line ->
          match try Some (String.index line ':') with Not_found -> None with
            | None ->
                send "stderr" "':' missing!";
                exit 1
            | Some idx ->
                Some (String.sub line 0 idx, String.sub line (idx + 1) (String.length line - (idx + 1)))

  let read_data () =
    let buf = Buffer.create 1024 in
    let rec loop first =
      match read_command () with
        | None ->
            send "stderr" "'end' command missing!";
            exit 1
        | Some ("data", data) ->
            if not first then Buffer.add_char buf '\n';
            Buffer.add_string buf data;
            loop false
        | Some ("end", _) ->
            Buffer.contents buf
        | Some (command, argument) ->
            Printf.ksprintf (send "stderr") "'data' or 'end' command expected, got %S!" command;
            exit 1
    in
    loop true

  let process_checked_phrase phrase =
    (* Rewrite toplevel expressions. *)
    let phrase = rewrite phrase in
    try
      Env.reset_cache_toplevel ();
      ignore (Toploop.execute_phrase true Format.std_formatter phrase);
      true
    with exn ->
      (* The only possible errors are directive errors. *)
      let msg = UTop.get_message Errors.report_error exn in
      (* Skip the dumb location. *)
      let msg =
        try
          let idx = String.index msg '\n' + 1 in
          String.sub msg idx (String.length msg - idx)
        with Not_found ->
          msg
      in
      List.iter (send "stderr") (split_at ~trim:true '\n' msg);
      false

  let process_input add_to_history eos_is_error =
    let input = read_data () in
    let result, warnings = parse_and_check input eos_is_error in
    match result with
      | UTop.Value phrase ->
          send "accept" "";
          List.iter (send "stderr") (split_at ~trim:true '\n' warnings);
          if add_to_history then LTerm_history.add UTop.history input;
          ignore (process_checked_phrase phrase)
      | UTop.Error (locs, msg) ->
          send "accept" (String.concat "," (List.map (fun (a, b) -> Printf.sprintf "%d,%d" a b) locs));
          List.iter (send "stderr") (split_at ~trim:true '\n' warnings);
          if add_to_history then LTerm_history.add UTop.history input;
          List.iter (send "stderr") (split_at ~trim:true '\n' msg)

  let send_error locs msg warnings =
    send "accept" (String.concat "," (List.map (fun (a, b) -> Printf.sprintf "%d,%d" a b) locs));
    match warnings with
      | Some warnings -> List.iter (send "stderr") (split_at ~trim:true '\n' warnings)
      | None -> ();
    List.iter (send "stderr") (split_at ~trim:true '\n' msg)

  let process_input_multi () =
    let input = read_data () in
    let result, warnings = parse_input_multi input in
    let typecheck phrase =
      match UTop.check_phrase phrase with
        | None -> None
        | Some (locs, msg) -> Some (convert_locs input locs, msg)  (* FIXME *)
    in
    match result with
      | UTop.Value phrases ->
          send "accept" "";
          List.iter (send "stderr") (split_at ~trim:true '\n' warnings);
          let rec loop = function
            | (phrase::more_phrases) -> begin
              match typecheck phrase with
                | Some (locs, msg) ->
                  send_error locs msg None
                | None ->
                  let success = process_checked_phrase phrase in
                  if success then
                    loop more_phrases
                  else
                    ()
              end
            | [] ->
              ()
          in
          loop phrases
      | UTop.Error (locs, msg) ->
        send_error locs msg (Some warnings)

  let rec loop () =
    (* Reset completion. *)
    UTop_complete.reset ();

    (* Increment the command counter. *)
    UTop_private.set_count (S.value UTop_private.count + 1);

    (* Call hooks. *)
    Lwt_sequence.iter_l (fun f -> f ()) UTop.new_command_hooks;

    (* Tell emacs we are ready. *)
    send "prompt" "";

    loop_commands (LTerm_history.contents UTop.history) []

  and loop_commands history_prev history_next =
    match read_command () with
      | None ->
          ()
      | Some ("input", arg) ->
          let args = split_at ',' arg in
          let allow_incomplete = List.mem "allow-incomplete" args
          and add_to_history = List.mem "add-to-history" args in
          let continue =
            try
              process_input add_to_history (not allow_incomplete);
              false
            with UTop.Need_more ->
              send "continue" "";
              true
          in
          if continue then
            loop_commands history_prev history_next
          else
            loop ()
      | Some ("input-multi", _) ->
          let continue =
            try
              process_input_multi ();
              false
            with UTop.Need_more ->
              send "continue" "";
              true
          in
          if continue then
            loop_commands history_prev history_next
          else
            loop ()
      | Some ("complete", _) ->
          let input = read_data () in
          let start, words =
            UTop_complete.complete
              ~syntax:(UTop.get_syntax ())
              ~phrase_terminator:(UTop.get_phrase_terminator ())
              ~input
          in
          let words = List.map fst words in
          let prefix = LTerm_read_line.common_prefix words in
          let index = String.length input - start in
          let suffix =
            if index > 0 && index <= String.length prefix then
              String.sub prefix index (String.length prefix - index)
            else
              ""
          in
          if suffix = "" then begin
            send "completion-start" "";
            List.iter (send "completion") words;
            send "completion-stop" "";
          end else
            send "completion-word" suffix;
          loop_commands history_prev history_next
      | Some ("history-prev", _) -> begin
          let input = read_data () in
          match history_prev with
            | [] ->
                send "history-bound" "";
                loop_commands history_prev history_next
            | entry :: history_prev ->
                List.iter (send "history-data") (split_at '\n' entry);
                send "history-end" "";
                loop_commands history_prev (input :: history_next)
        end
      | Some ("history-next", _) -> begin
          let input = read_data () in
          match history_next with
            | [] ->
                send "history-bound" "";
                loop_commands history_prev history_next
            | entry :: history_next ->
                List.iter (send "history-data") (split_at '\n' entry);
                send "history-end" "";
                loop_commands (input :: history_prev) history_next
        end
      | Some ("exit", code) ->
          exit (int_of_string code)
      | Some ("save-history", code) ->
          Lwt_main.run (save_history ());
          loop_commands history_prev history_next
      | Some ("require", package) -> begin
        try
          Topfind.load_deeply [package]
        with Fl_package_base.No_such_package(pkg, reason) ->
          send "no-such-package" pkg
      end;
        loop_commands history_prev history_next
      | Some (command, _) ->
          Printf.ksprintf (send "stderr") "unrecognized command %S!" command;
          exit 1
end

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let emacs_mode = ref false
let preload_objects = ref []

let prepare () =
  Toploop.set_paths ();
  try
    let res = List.for_all (Topdirs.load_file Format.err_formatter) (List.rev !preload_objects) in
    !Toploop.toplevel_startup_hook ();
    res
  with exn ->
    try
      Errors.report_error Format.err_formatter exn;
      false
    with exn ->
      Format.eprintf "Uncaught exception: %s\n" (Printexc.to_string exn);
      false

let read_script_from_stdin () =
  let args = Array.sub Sys.argv !Arg.current (Array.length Sys.argv - !Arg.current) in
  if prepare () && Toploop.run_script Format.err_formatter "" args then
    exit 0
  else
    exit 2

let file_argument name =
  if Filename.check_suffix name ".cmo" || Filename.check_suffix name ".cma" then
    preload_objects := name :: !preload_objects
  else begin
    let args = Array.sub Sys.argv !Arg.current (Array.length Sys.argv - !Arg.current) in
    if prepare () && Toploop.run_script Format.err_formatter name args then
      exit 0
    else
      exit 2
  end

let print_version () =
  Printf.printf "The universal toplevel for OCaml, version %s, compiled for OCaml version %s\n" UTop.version Sys.ocaml_version;
  exit 0

let print_version_num () =
  Printf.printf "%s\n" UTop.version

let args = Arg.align [
#if ocaml_version >= (3, 13, 0)
  "-absname", Arg.Set Location.absname, " Show absolute filenames in error message";
#endif
  "-I", Arg.String (fun dir ->  Clflags.include_dirs := Misc.expand_directory Config.standard_library dir :: !Clflags.include_dirs), "<dir> Add <dir> to the list of include directories";
  "-init", Arg.String (fun s -> Clflags.init_file := Some s), "<file> Load <file> instead of default init file";
  "-labels", Arg.Clear Clflags.classic, " Use commuting label mode";
  "-no-app-funct", Arg.Clear Clflags.applicative_functors, " Deactivate applicative functors";
  "-noassert", Arg.Set Clflags.noassert, " Do not compile assertion checks";
  "-nolabels", Arg.Set Clflags.classic, " Ignore non-optional labels in types";
  "-nostdlib", Arg.Set Clflags.no_std_include, " Do not add default directory to the list of include directories";
  "-principal", Arg.Set Clflags.principal, " Check principality of type inference";
#if ocaml_version >= (4, 01, 0)
  "-short-paths", Arg.Clear Clflags.real_paths, " Shorten paths in types (the default)";
  "-no-short-paths", Arg.Set Clflags.real_paths, " Do not shorten paths in types";
#endif
  "-rectypes", Arg.Set Clflags.recursive_types, " Allow arbitrary recursive types";
  "-stdin", Arg.Unit read_script_from_stdin, " Read script from standard input";
  "-strict-sequence", Arg.Set Clflags.strict_sequence, " Left-hand part of a sequence must have type unit";
  "-unsafe", Arg.Set Clflags.fast, " Do not compile bounds checking on array and string access";
  "-version", Arg.Unit print_version, " Print version and exit";
  "-vnum", Arg.Unit print_version_num, " Print version number and exit";
  "-w", Arg.String (Warnings.parse_options false),
  Printf.sprintf
    "<list>  Enable or disable warnings according to <list>:\n\
    \        +<spec>   enable warnings in <spec>\n\
    \        -<spec>   disable warnings in <spec>\n\
    \        @<spec>   enable warnings in <spec> and treat them as errors\n\
    \     <spec> can be:\n\
    \        <num>             a single warning number\n\
    \        <num1>..<num2>    a range of consecutive warning numbers\n\
    \        <letter>          a predefined set\n\
    \     default setting is %S" Warnings.defaults_w;
  "-warn-error", Arg.String (Warnings.parse_options true),
  Printf.sprintf
    "<list>  Enable or disable error status for warnings according to <list>\n\
    \     See option -w for the syntax of <list>.\n\
    \     Default setting is %S" Warnings.defaults_warn_error;
  "-warn-help", Arg.Unit Warnings.help_warnings, " Show description of warning numbers";
  "-emacs", Arg.Set emacs_mode, " Run in emacs mode";
  "-hide-reserved", Arg.Unit (fun () -> UTop.set_hide_reserved true),
  " Hide identifiers starting with a '_' (the default)";
  "-show-reserved", Arg.Unit (fun () -> UTop.set_hide_reserved false),
  " Show identifiers starting with a '_'";
]

#if ocaml_version >= (4, 01, 0)
let () = Clflags.real_paths := false
#endif

let app_name = Filename.basename Sys.executable_name
let usage = Printf.sprintf "Usage: %s <options> <object-files> [script-file [arguments]]\noptions are:" app_name

let common_init () =
  (* Initializes toplevel environment. *)
  Toploop.initialize_toplevel_env ();
  (* Set the global input name. *)
  Location.input_name := UTop.input_name;
  (* Make sure SIGINT is catched while executing OCaml code. *)
  Sys.catch_break true;
  (* Load user's .ocamlinit file. *)
  (match !Clflags.init_file with
     | Some fn ->
         if Sys.file_exists fn then
           ignore (Toploop.use_silently Format.err_formatter fn)
         else
           Printf.eprintf "Init file not found: \"%s\".\n" fn
     | None ->
         if Sys.file_exists ".ocamlinit" then
           ignore (Toploop.use_silently Format.err_formatter ".ocamlinit")
         else
           let fn = Filename.concat LTerm_resources.home ".ocamlinit" in
           if Sys.file_exists fn then
             ignore (Toploop.use_silently Format.err_formatter fn));
  (* Load history after the initialization file so the user can change
     the history file name. *)
  Lwt_main.run (init_history ());
  (* Install signal handlers. *)
  let behavior = Sys.Signal_handle (fun signo -> raise (Term signo)) in
  let catch signo =
    try
      Sys.set_signal signo behavior
    with _ ->
      (* All signals may not be supported on some OS. *)
      ()
  in
  (* We lost the terminal. *)
  catch Sys.sighup;
  (* Termination request. *)
  catch Sys.sigterm

let load_inputrc () =
  try_lwt
    LTerm_inputrc.load ()
  with
  | Unix.Unix_error (error, func, arg) ->
    Lwt_log.error_f "cannot load key bindings from %S: %s: %s" LTerm_inputrc.default func (Unix.error_message error)
  | LTerm_inputrc.Parse_error (fname, line, msg) ->
    Lwt_log.error_f "error in key bindings file %S, line %d: %s" fname line msg

let main_aux () =
  Arg.parse args file_argument usage;
  if not (prepare ()) then exit 2;
  if !emacs_mode then begin
    UTop_private.set_ui UTop_private.Emacs;
    let module Emacs = Emacs (struct end) in
    Printf.printf "Welcome to utop version %s (using OCaml version %s)!\n\n%!" UTop.version Sys.ocaml_version;
    common_init ();
    Emacs.loop ()
  end else begin
    UTop_private.set_ui UTop_private.Console;
    let term = Lwt_main.run (Lazy.force LTerm.stdout) in
    if LTerm.incoming_is_a_tty term && LTerm.outgoing_is_a_tty term then begin
      (* Set the initial size. *)
      UTop_private.set_size (S.const (LTerm.size term));
      (* Load user data. *)
      Lwt_main.run (join [UTop_styles.load (); load_inputrc ()]);
      (* Display a welcome message. *)
      Lwt_main.run (welcome term);
      (* Common initialization. *)
      common_init ();
      (* Print help message. *)
      print_string "\nType #utop_help for help about using utop.\n\n";
      flush stdout;
      (* Main loop. *)
      try
        loop term
      with LTerm_read_line.Interrupt ->
        ()
    end else begin
      (* Use the standard toplevel. Just make sure that Lwt threads can
         run while reading phrases. *)
      Toploop.read_interactive_input := read_input_classic;
      Toploop.loop Format.std_formatter
    end
  end;
  (* Don't let the standard toplevel run... *)
  exit 0

let main () =
  try
    main_aux ()
  with exn ->
    (match exn with
       | Unix.Unix_error (error, func, "") ->
           Printf.eprintf "%s: %s: %s\n" app_name func (Unix.error_message error)
       | Unix.Unix_error (error, func, arg) ->
           Printf.eprintf "%s: %s(%S): %s\n" app_name func arg (Unix.error_message error)
       | exn ->
           Printf.eprintf "Fatal error: exception %s\n" (Printexc.to_string exn));
    Printexc.print_backtrace stderr;
    flush stderr;
    exit 2
