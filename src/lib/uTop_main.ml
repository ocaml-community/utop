(*
 * uTop_main.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

[@@@warning "-7-9-27-32-33"]

open Lwt_react
open LTerm_dlist
open LTerm_text
open LTerm_geom
open UTop
open UTop_compat
open UTop_token
open UTop_styles
open UTop_private
open UTop.Private

let return, (>>=) = Lwt.return, Lwt.(>>=)

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
        Lwt.catch
          (fun () -> LTerm_history.save UTop.history
                        ?max_size:!UTop.history_file_max_size
                        ?max_entries:!UTop.history_file_max_entries fn)
          (function
          | Unix.Unix_error (error, func, arg) ->
              Logs_lwt.err (fun m -> m "cannot save history to %S: %s: %s" fn func (Unix.error_message error))
          | exn -> Lwt.fail exn)

let init_history () =
  (* Save history on exit. *)
  Lwt_main.at_exit save_history;
  (* Load history. *)
  match !UTop.history_file_name with
    | None ->
        return ()
    | Some fn ->
        Lwt.catch
          (fun () -> LTerm_history.load UTop.history fn)
          (function
          | Unix.Unix_error (error, func, arg) ->
              Logs_lwt.err (fun m -> m "cannot load history from %S: %s: %s"
                                fn func (Unix.error_message error))
          | exn -> Lwt.fail exn)

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

let convert_loc str (a, b) = (index_of_offset str a, index_of_offset str b)

let convert_locs str locs = List.map (fun (a, b) -> convert_loc str (a,b)) locs

let get_line src line =
  let rec aux line' ofs skipped =
    if ofs >= String.length src then
      ("", 0)
    else if line' = line then
      (String.sub src ofs (String.length src - ofs), skipped)
    else
      let ch, next_ofs = Zed_utf8.unsafe_extract_next src ofs in
      if Zed_utf8.escaped_char ch = "\\n" then
        aux (line' + 1) next_ofs (skipped + 1)
      else
        aux line' next_ofs (skipped + 1)
  in
  aux 1 0 0

let convert_one_line str line ofs=
  let selected, skipped = get_line str line in
  index_of_offset selected ofs + skipped

let convert_line str (start_ofs, end_ofs) lines =
  (convert_one_line str lines.start start_ofs,
  convert_one_line str lines.stop end_ofs)

let convert_loc_line input locs lines =
  List.map2 (fun loc line ->
    match line with
    | None ->
      convert_loc input loc
    | Some line ->
      convert_line input loc line) locs lines

(* +-----------------------------------------------------------------+
   | The read-line class                                             |
   +-----------------------------------------------------------------+ *)

let preprocess input =
  match input with
    | Parsetree.Ptop_def pstr ->
        Parsetree.Ptop_def
          (Pparse.apply_rewriters ~tool_name:"ocaml" Pparse.Structure pstr)
    | _ -> input

let parse_input_multi input =
  let buf = Buffer.create 32 in
  let result =
    UTop.collect_formatters buf [Format.err_formatter]
      (fun () ->
         match !UTop.parse_use_file input false with
           | UTop.Error (locs, msg) ->
               UTop.Error (convert_locs input locs, "Error: " ^ msg ^ "\n")
           | UTop.Value phrases ->
               try
                 UTop.Value (List.map preprocess phrases)
               with Pparse.Error error ->
                 Pparse.report_error Format.str_formatter error;
                 UTop.Error ([], "Error: " ^ Format.flush_str_formatter () ^ "\n"))
  in
  (result, Buffer.contents buf)

let parse_and_check input ~eos_is_error =
  let buf = Buffer.create 32 in
  let result =
    UTop.collect_formatters buf [Format.err_formatter]
      (fun () ->
         match !UTop.parse_toplevel_phrase input eos_is_error with
           | UTop.Error (locs, msg) ->
               UTop.Error (convert_locs input locs, "Error: " ^ msg ^ "\n")
           | UTop.Value phrase ->
               try
                 let phrase = preprocess phrase in
                 match UTop.check_phrase phrase with
                   | None ->
                       UTop.Value phrase
                   | Some (locs, msg, lines) ->
                       UTop.Error (convert_loc_line input locs lines, msg)
               with Pparse.Error error ->
                 Pparse.report_error Format.str_formatter error;
                 UTop.Error ([], "Error: " ^ Format.flush_str_formatter () ^ "\n"))
  in
  (result, Buffer.contents buf)

let add_terminator s =
  let terminator = UTop.get_phrase_terminator () |> Zed_string.unsafe_of_utf8 in
  if Zed_string.ends_with s ~suffix:terminator then
    s
  else
    Zed_string.append s terminator

let is_accept : LTerm_read_line.action -> bool = function
  | Accept -> true
  | action -> action == UTop.end_and_accept_current_phrase

(* Read a phrase. If the result is a value, it is guaranteed to be a
   valid phrase (i.e. typable and compilable). It also returns
   warnings printed parsing. *)
class read_phrase ~term = object(self)
  inherit [Parsetree.toplevel_phrase UTop.result * string] LTerm_read_line.engine ~history:(LTerm_history.contents UTop.history) () as super
  inherit [Parsetree.toplevel_phrase UTop.result * string] LTerm_read_line.term term as super_term

  method create_temporary_file_for_external_editor =
    Filename.temp_file "utop" ".ml"

  method external_editor = UTop.get_external_editor ()

  val mutable return_value = None

  method eval =
    match return_value with
    | Some x ->
      x
    | None -> assert false

  method! send_action action =
    let action : LTerm_read_line.action =
      if is_accept action && S.value self#mode <> LTerm_read_line.Edition then
        Accept
      else
        action
    in
    super#send_action action

  method! exec ?(keys=[]) = function
    | action :: actions when S.value self#mode = LTerm_read_line.Edition &&
                             is_accept action  -> begin
        Zed_macro.add self#macro action;
        let input = Zed_rope.to_string (Zed_edit.text self#edit) in
        let input =
          if action == UTop.end_and_accept_current_phrase then
            add_terminator input
          else
            input
        in
        let input_utf8= Zed_string.to_utf8 input in
        (* Toploop does that: *)
        Location.reset ();
        try
          let result = parse_and_check input_utf8 ~eos_is_error:false in
          return_value <- Some result;
          LTerm_history.add UTop.history input;
          let out, warnings = result in
          begin
            match out with
            | UTop.Value _ ->
              UTop_history.add_input UTop.stashable_session_history input_utf8;
              UTop_history.add_warnings UTop.stashable_session_history warnings;
            | (UTop.Error (_, msg)) ->
              UTop_history.add_bad_input UTop.stashable_session_history input_utf8;
              UTop_history.add_warnings UTop.stashable_session_history warnings;
              UTop_history.add_error UTop.stashable_session_history msg;
          end;
          return (LTerm_read_line.Result result)
        with UTop.Need_more ->
          (* Input not finished, continue. *)
          self#insert (Uchar.of_char '\n');
          self#exec ~keys actions
      end
    | actions ->
      super_term#exec actions

  method! stylise last =
    let styled, position = super#stylise last in

    (* Syntax highlighting *)
    let stylise loc token_style =
      for i = loc.idx1 to loc.idx2 - 1 do
        let ch, style = styled.(i) in
        styled.(i) <- (ch, LTerm_style.merge token_style style)
      done
    in
    UTop_styles.stylise stylise (UTop_lexer.lex_string (Zed_string.to_utf8 (LTerm_text.to_string styled)));

    if not last then
      (* Parenthesis matching. *)
      LTerm_text.stylise_parenthesis styled position styles.style_paren
    else begin
      match return_value with
      | Some (UTop.Error (locs, _), _) ->
        (* Highlight error locations. *)
        List.iter
          (fun (start, stop) ->
             for i = max 0 start to min (Array.length styled) stop - 1 do
               let ch, style = styled.(i) in
               styled.(i) <- (ch, { style with LTerm_style.underline = Some true })
             done)
          locs
      | _ ->
        ()
    end;

    (styled, position)

  method! completion =
    let pos, words =
      UTop_complete.complete
        ~phrase_terminator:(UTop.get_phrase_terminator ())
        ~input:(Zed_string.to_utf8 (Zed_rope.to_string self#input_prev))
    in
    let words= words |> List.map (fun (k, v)->
      (Zed_string.unsafe_of_utf8 k, Zed_string.unsafe_of_utf8 v)) in
    self#set_completion pos words

  method! show_box = S.value self#mode <> LTerm_read_line.Edition || UTop.get_show_box ()

  initializer
    (* Set the source signal for the size of the terminal. *)
    UTop_private.set_size self#size;
    (* Set the source signal for the key sequence. *)
    UTop_private.set_key_sequence self#key_sequence;
    (* Set the prompt. *)
    self#set_prompt !UTop.prompt
end

(* +-----------------------------------------------------------------+
   | Handling of [@@toplevel_printer] attributes                     |
   +-----------------------------------------------------------------+ *)

module Autoprinter : sig
  val scan_env : Format.formatter -> unit

  val scan_cmis : Format.formatter -> unit
end = struct
  open Types

  let cons_path path id =
    let comp = Ident.name id in
    match path with
    | None -> Longident.Lident comp
    | Some path -> Longident.Ldot (path, comp)

  let is_auto_printer_attribute (attr : Parsetree.attribute) =
    let name = attr.attr_name in
    match name.txt with
    | "toplevel_printer" | "ocaml.toplevel_printer" -> true
    | _ -> false

  let rec walk_sig pp ~path signature =
    List.iter (walk_sig_item pp (Some path)) signature

  and walk_sig_item pp path = function
    | Sig_module (id, _, {md_type = mty; _}, _, _) ->
        walk_mty pp (cons_path path id) mty
    | Sig_value (id, vd, _) ->
      if List.exists is_auto_printer_attribute vd.val_attributes then
        Topdirs.dir_install_printer pp (cons_path path id)
    | _ -> ()

  and walk_mty pp path = function
    | Mty_signature s -> walk_sig pp ~path s
    | _ -> ()

  let scan_cmis =
    let new_cmis = ref [] in
    UTop_compat.add_cmi_hook (fun cmi -> new_cmis := cmi :: !new_cmis );
    fun pp ->
      List.iter (fun (cmi : Cmi_format.cmi_infos) ->
        walk_sig pp ~path:(Longident.Lident cmi.cmi_name) cmi.cmi_sign
      ) !new_cmis;
      new_cmis := []

  let scan_env =
    let last_globals = ref (Env.get_required_globals ()) in
    let last_summary = ref Env.Env_empty in
    fun pp ->
      let env = !Toploop.toplevel_env in
      let scan_module env id =
        let name = Longident.Lident (Ident.name id) in
        let path, {md_type; _} = Env.find_module_by_name name env in
        if path = Path.Pident id then
          walk_mty pp name md_type
      in
      let rec scan_globals last = function
        | [] -> ()
        | x when x == last -> ()
        | x :: xs ->
          scan_globals last xs;
          scan_module env x
      in
      let rec scan_summary last = function
        | Env.Env_empty -> ()
        | x when x == last -> ()
        | Env.Env_module (s, id, _, _) ->
          scan_summary last s;
          scan_module env id
        | Env.Env_copy_types s
        | Env.Env_value_unbound (s, _, _)
        | Env.Env_module_unbound (s, _, _)
        | Env.Env_persistent (s, _)
        | Env.Env_value (s, _, _)
        | Env.Env_type (s, _, _)
        | Env.Env_extension (s, _, _)
        | Env.Env_modtype (s, _, _)
        | Env.Env_class (s, _, _)
        | Env.Env_cltype (s, _, _)
        | Env.Env_open (s, _)
        | Env.Env_functor_arg (s, _)
        | Env.Env_constraints (s, _) ->
          scan_summary last s
      in
      let globals = Env.get_required_globals () in
      let last_globals' = !last_globals in
      last_globals := globals;
      scan_globals last_globals' globals;
      let summary = Env.summary env in
      let last_summary' = !last_summary in
      last_summary := summary;
      scan_summary last_summary' summary
end

let render_out_phrase term string =
  if String.length string >= 100 * 1024 then
    LTerm.fprint term string
  else begin
    let string = fix_string string in
    let styled = LTerm_text.of_utf8 string in
    let stylise loc token_style =
      for i = loc.idx1 to loc.idx2 - 1 do
        let ch, style = styled.(i) in
        styled.(i) <- (ch, LTerm_style.merge token_style style)
      done
    in
    UTop_styles.stylise stylise (UTop_lexer.lex_string string);
    LTerm.fprints term styled
  end

let orig_print_out_signature = !Toploop.print_out_signature
let orig_print_out_phrase = !Toploop.print_out_phrase

let is_implicit_name name =
  name <> "" &&
  name.[0] = '_' &&
  try
    let _ = int_of_string @@ String.sub name 1 (String.length name - 1) in
    true
  with
    Failure _ -> false

let map_items unwrap wrap items =
  let rec aux acc = function
    | [] ->
       acc
    | item :: items ->
       let sig_item, _ = unwrap item in
       let name, rec_status =
         match sig_item with
         | Outcometree.Osig_class (_, name, _, _, rs)
         | Outcometree.Osig_class_type (_, name, _, _, rs)
         | Outcometree.Osig_module (name, _, rs)
         | Outcometree.Osig_type ({ Outcometree.otype_name = name }, rs) ->
            (name, rs)
         | Outcometree.Osig_typext ({ Outcometree.oext_name = name}, _)
         | Outcometree.Osig_modtype (name, _)
         | Outcometree.Osig_value { oval_name = name; _ } ->
            (name, Outcometree.Orec_not)
         | Outcometree.Osig_ellipsis -> ("", Outcometree.Orec_not)
       in
       let keep =
         name = "" || name.[0] <> '_' ||
           (UTop.get_create_implicits () && is_implicit_name name)
       in
       if keep then
         aux (item :: acc) items
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
              | Outcometree.Osig_type (oty, rs) ->
                 if rs = Outcometree.Orec_next then
                   wrap (Outcometree.Osig_type (oty, Outcometree.Orec_first)) extra :: items'
                 else
                   items
              | Outcometree.Osig_typext _
              | Outcometree.Osig_ellipsis
              | Outcometree.Osig_modtype _
              | Outcometree.Osig_value _ ->
                 items
         in
         aux acc items
  in
  List.rev (aux [] items)

let print_out_signature pp items =
  if UTop.get_hide_reserved () then
    orig_print_out_signature pp (map_items (fun x -> (x, ())) (fun x () -> x) items)
  else
    orig_print_out_signature pp items

let print_out_phrase pp phrase =
  Autoprinter.scan_env pp;
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

let with_loc loc str = {
  Location.txt = str;
  Location.loc = loc;
}

(* A rule for rewriting a toplevel expression. *)
type rewrite_rule = {
  type_to_rewrite : Longident.t;
  mutable path_to_rewrite : Path.t option;
  required_values : Longident.t list;
  (* Values that must exist and be persistent for the rule to apply. *)
  rewrite : Location.t -> Parsetree.expression -> Parsetree.expression;
  (* The rewrite function. *)
  enabled : bool React.signal;
  (* Whether the rule is enabled or not. *)
}

let longident_lwt_main_run = Longident.Ldot (Longident.Lident "Lwt_main", "run")
let longident_async_thread_safe_block_on_async_exn =
  Longident.(Ldot (Ldot (Lident "Async", "Thread_safe"), "block_on_async_exn"))

let rewrite_rules = [
  (* Rewrite Lwt.t expressions to Lwt_main.run <expr> *)
  {
    type_to_rewrite = Longident.(Ldot (Lident "Lwt", "t"));
    path_to_rewrite = None;
    required_values = [longident_lwt_main_run];
    rewrite = (fun loc e ->
      let open Ast_helper in
      with_default_loc loc (fun () ->
        Exp.apply (Exp.ident (with_loc loc longident_lwt_main_run)) [(Nolabel, e)]
      )
    );
    enabled = UTop.auto_run_lwt;
  };

  (* Rewrite Async.Defered.t expressions to
     Async.Thread_safe.block_on_async_exn (fun () -> <expr>). *)
  {
    type_to_rewrite = Longident.(Ldot (Ldot (Lident "Async", "Deferred"), "t"));
    path_to_rewrite = None;
    required_values = [longident_async_thread_safe_block_on_async_exn];
    rewrite = (fun loc e ->
      let open Ast_helper in
      let punit = Pat.construct (with_loc loc (Longident.Lident "()")) None in
      with_default_loc loc (fun () ->
        Exp.apply
          (Exp.ident (with_loc loc longident_async_thread_safe_block_on_async_exn))
          [(Nolabel, UTop_compat.Exp.fun_ ~loc punit e)]
      )
    );
    enabled = UTop.auto_run_async;
  }
]

let rule_path rule =
  match rule.path_to_rewrite with
  | Some _ as x -> x
  | None ->
    try
      let env = !Toploop.toplevel_env in
      let path =
        match Env.find_type_by_name rule.type_to_rewrite env with
        | path, { Types.type_kind     = type_kind
                ; Types.type_private  = Asttypes.Public
                ; Types.type_manifest = Some ty
                } when type_kind = UTop_compat.abstract_type_kind -> begin
            match get_desc (Ctype.expand_head env ty) with
            | Types.Tconstr (path, _, _) -> path
            | _ -> path
          end
        | path, _ -> path
      in
      let opt = Some path in
      rule.path_to_rewrite <- opt;
      opt
    with _ ->
      None

(* Check that the given long identifier is present in the environment
   and is persistent. *)
let is_persistent_in_env longident =
  try
    is_persistent_path (fst (Env.find_value_by_name longident !Toploop.toplevel_env))
  with Not_found ->
    false

let rule_matches rule path =
  React.S.value rule.enabled &&
  (match rule_path rule with
   | None -> false
   | Some path' -> Path.same path path') &&
  List.for_all is_persistent_in_env rule.required_values

(* Returns whether the argument is a toplevel expression. *)
let is_eval = function
  | { Parsetree.pstr_desc = Parsetree.Pstr_eval _ } -> true
  | _ -> false

(* Returns the rewrite rule associated to a type, if any. *)
let rule_of_type typ =
  match get_desc (Ctype.expand_head !Toploop.toplevel_env typ) with
  | Types.Tconstr (path, _, _) -> begin
      try
        Some (List.find (fun rule -> rule_matches rule path) rewrite_rules)
      with _ ->
        None
    end
  | _ ->
    None

let rewrite_str_item pstr_item tstr_item =
  match pstr_item, tstr_item.Typedtree.str_desc with
    | ({ Parsetree.pstr_desc = Parsetree.Pstr_eval (e, _);
         Parsetree.pstr_loc = loc },
       Typedtree.Tstr_eval ({ Typedtree.exp_type = typ }, _)) -> begin
      match rule_of_type typ with
        | Some rule ->
          { Parsetree.pstr_desc = Parsetree.Pstr_eval (rule.rewrite loc e, []);
            Parsetree.pstr_loc = loc }
        | None ->
          pstr_item
    end
    | _ ->
      pstr_item

let type_structure env pstr =
#if OCAML_VERSION >= (4, 14, 0)
  let tstr, _, _, _, _ = Typemod.type_structure env pstr in
#elif OCAML_VERSION >= (4, 12, 0)
  let tstr, _, _, _ = Typemod.type_structure env pstr in
#else
  let tstr, _, _, _ = Typemod.type_structure env pstr Location.none in
#endif
  tstr

let rewrite phrase =
  match phrase with
    | Parsetree.Ptop_def pstr ->
      if (UTop.get_auto_run_lwt () || UTop.get_auto_run_async ()) && List.exists is_eval pstr then
        let tstr = type_structure !Toploop.toplevel_env pstr in
        Parsetree.Ptop_def (List.map2 rewrite_str_item pstr tstr.Typedtree.str_items)
      else
        phrase
    | Parsetree.Ptop_dir _ ->
      phrase

let add_let binding_name def =
  let open Parsetree in
  match def with
  | { pstr_desc = Pstr_eval (expr, attrs); pstr_loc = loc } ->
      Ast_helper.Str.value ~loc Nonrecursive
      [
        let pat = Ast_helper.Pat.var ~loc { txt = binding_name; loc } in
        Ast_helper.Vb.mk ~loc ~attrs pat expr
      ]
  | _ ->
    def

let bind_expressions name phrase =
  match phrase with
    | Parsetree.Ptop_def pstr ->
      Parsetree.Ptop_def (List.map (add_let name) pstr)
    | Parsetree.Ptop_dir _ ->
      phrase

let execute_phrase b ppf phrase =
  Autoprinter.scan_cmis ppf;
  let res = Toploop.execute_phrase b ppf phrase in
  Autoprinter.scan_cmis ppf;
  res

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let registers= ref LTerm_vi.Vi.Interpret.RegisterMap.empty

let rec read_phrase term =
  Lwt.catch
    (fun () ->
      let read_line= new read_phrase ~term in
      (match !UTop.edit_mode with
      | LTerm_editor.Default-> ()
      | LTerm_editor.Vi as mode-> read_line#set_editor_mode mode);
      let vi_state= read_line#vi_state in
      vi_state#set_registers !registers;
      read_line#run >>= fun result->
      registers:= vi_state#get_registers;
      return result)
    (function
    | Sys.Break ->
      LTerm.fprintl term "Interrupted." >>= fun () ->
      read_phrase term
    | exn -> Lwt.fail exn)

let print_error term msg =
  LTerm.set_style term styles.style_error >>= fun () ->
  Lwt_io.print msg >>= fun () ->
  LTerm.set_style term LTerm_style.none >>= fun () ->
  LTerm.flush term

let rec loop term =
  (* Reset completion. *)
  UTop_complete.reset ();

  (* increment the command counter. *)
  UTop_private.set_count (S.value UTop_private.count + 1);

  (* Call hooks. *)
  LTerm_dlist.iter_l (fun f -> f ()) UTop.new_command_hooks;

  (* Read interactively user input. *)
  let phrase_opt =
    Lwt_main.run (
      Lwt.finalize
        (fun () ->
          read_phrase term >>= fun (result, warnings) ->
          (* Print warnings before errors. *)
          Lwt_io.print warnings >>= fun () ->
          match result with
            | UTop.Value phrase ->
                return (Some phrase)
            | UTop.Error (locs, msg) ->
                print_error term msg >>= fun () ->
                return None)
        (fun () -> LTerm.flush term)
    )
  in
  match phrase_opt with
    | Some phrase ->
        (* Rewrite toplevel expressions. *)
        let count = S.value UTop_private.count in
        let phrase = rewrite phrase in
        let phrase =
          if UTop.get_create_implicits () then
            let binding_name = Printf.sprintf "_%d" count in
            bind_expressions binding_name phrase
          else
            phrase
        in
        (* Set the margin of standard formatters. *)
        UTop_private.set_margin Format.std_formatter;
        UTop_private.set_margin Format.err_formatter;
        (* Formatter to get the output phrase. *)
        let buffer = Buffer.create 1024 in
        let pp = Format.formatter_of_buffer buffer in
        UTop_private.set_margin pp;
        (try
           Env.reset_cache_toplevel ();
           if !Clflags.dump_parsetree then Printast.top_phrase pp phrase;
           if !Clflags.dump_source then Pprintast.top_phrase pp phrase;
           ignore (execute_phrase true pp phrase);
           (* Flush everything. *)
           Format.pp_print_flush Format.std_formatter ();
           Format.pp_print_flush Format.err_formatter ();
           flush stdout;
           flush stderr;
           (* Get the string printed. *)
           Format.pp_print_flush pp ();
           let string = Buffer.contents buffer in
           UTop_history.add_output UTop.stashable_session_history string;
           match phrase with
             | Parsetree.Ptop_def _ ->
                 (* The string is an output phrase, colorize it. *)
                 Lwt_main.run (render_out_phrase term string)
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
  LTerm.print_box term matrix >>= fun () ->

  (* Move to after the box. *)
  LTerm.fprint term "\n" >>= fun () ->

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
            Bytes.set buffer i c;
            if c = '\n' then
              return (i + 1, false)
            else
              loop (i + 1)
        | None ->
            return (i, true)
  in
  Lwt_main.run (Lwt_io.write Lwt_io.stdout prompt >>= fun () -> loop 0)

(* +-----------------------------------------------------------------+
   | Emacs mode                                                      |
   +-----------------------------------------------------------------+ *)

module Emacs(M : sig end) = struct

  (* Copy standard output, which will be used to send commands. *)
  let command_oc = Unix.out_channel_of_descr (Unix.dup Unix.stdout)

  let split_at ?(trim=false) ch str =
    let rec aux acc i j =
      if j = String.length str then
        if trim && i = j then
          acc
        else
          (String.sub str i (j - i)) :: acc
      else if str.[j] = ch then
        aux (String.sub str i (j - i) :: acc) (j + 1) (j + 1)
      else
        aux acc i (j + 1)
    in
    List.rev (aux [] 0 0)

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
      ignore (execute_phrase true Format.std_formatter phrase);
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
    let input_zed= Zed_string.unsafe_of_utf8 input in
    let result, warnings = parse_and_check input ~eos_is_error in
    match result with
      | UTop.Value phrase ->
          send "accept" "";
          List.iter (send "stderr") (split_at ~trim:true '\n' warnings);
          if add_to_history then LTerm_history.add UTop.history input_zed;
          ignore (process_checked_phrase phrase)
      | UTop.Error (locs, msg) ->
          send "accept" (String.concat "," (List.map (fun (a, b) -> Printf.sprintf "%d,%d" a b) locs));
          List.iter (send "stderr") (split_at ~trim:true '\n' warnings);
          if add_to_history then LTerm_history.add UTop.history input_zed;
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
        | Some (locs, msg, lines) -> Some (convert_loc_line input locs lines, msg)
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
    LTerm_dlist.iter_l (fun f -> f ()) UTop.new_command_hooks;

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
      | Some ("complete-company", _) ->
        let input = read_data () in
        let _, words =
          UTop_complete.complete
            ~phrase_terminator:(UTop.get_phrase_terminator ())
            ~input
        in
        send "completion-start" "";
        List.iter (fun (w, _) -> send "completion" w) words;
        send "completion-stop" "";
        loop_commands history_prev history_next
      | Some ("complete", _) ->
          let input = read_data () in
          let start, words =
            UTop_complete.complete
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
                List.iter (send "history-data") (split_at '\n' (Zed_string.to_utf8 entry));
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
                loop_commands ((Zed_string.unsafe_of_utf8 input) :: history_prev) history_next
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
   | Extra macros                                                    |
   +-----------------------------------------------------------------+ *)

let typeof sid =
  let id = Parse.longident (Lexing.from_string sid) in
  let env = !Toploop.toplevel_env in
  let from_type_desc = function
    | Types.Tconstr (path, _, _) ->
      let typ_decl = Env.find_type path env in
      path, typ_decl
    | _ -> assert false
  in
  let out_sig_item =
    try
      let (path, ty_decl) = Env.find_type_by_name id env in
      let id = Ident.create_local (Path.name path) in
      Some (Printtyp.tree_of_type_declaration id ty_decl Types.Trec_not)
    with Not_found ->
    try
      let (path, val_descr) = Env.find_value_by_name id env in
      let id = Ident.create_local (Path.name path) in
      Some (Printtyp.tree_of_value_description id val_descr)
    with Not_found ->
    try
      let lbl_desc = Env.find_label_by_name id env in
      let (path, ty_decl) = from_type_desc (get_desc lbl_desc.Types.lbl_res) in
      let id = Ident.create_local (Path.name path) in
      Some (Printtyp.tree_of_type_declaration id ty_decl Types.Trec_not)
    with Not_found ->
    try
      let path, {Types.md_type; _} = Env.find_module_by_name id env in
      let id = Ident.create_local (Path.name path) in
      Some (Printtyp.tree_of_module id md_type Types.Trec_not)
    with Not_found ->
    try
      let (path, mty_decl) = Env.find_modtype_by_name id env in
      let id = Ident.create_local (Path.name path) in
      Some (Printtyp.tree_of_modtype_declaration id mty_decl)
    with Not_found ->
    try
      let cstr_desc = Env.find_constructor_by_name id env in
      match cstr_desc.Types.cstr_tag with
      | _ ->
        let (path, ty_decl) = from_type_desc (get_desc cstr_desc.Types.cstr_res) in
        let id = Ident.create_local (Path.name path) in
        Some (Printtyp.tree_of_type_declaration id ty_decl Types.Trec_not)
    with Not_found ->
      None
  in
  match out_sig_item with
  | None ->
    Lwt_main.run (Lazy.force LTerm.stdout >>= fun term ->
                  print_error term "Unknown type\n")
  | Some osig ->
    let buf = Buffer.create 128 in
    let pp = Format.formatter_of_buffer buf in
    !Toploop.print_out_signature pp [osig];
    Format.pp_print_newline pp ();
    let str = Buffer.contents buf in
    Lwt_main.run (Lazy.force LTerm.stdout >>= fun term -> render_out_phrase term str)

let default_info = {
  Toploop.section = "UTop";
  doc = ""; (* TODO: have some kind of documentation *)
}

let () =
  Toploop.add_directive "typeof"
    (Toploop.Directive_string typeof)
    default_info

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let emacs_mode = ref false
let preload = ref []

let prepare () =
  toploop_set_paths ();
  try
    let ok =
      List.for_all
        (function
          | `Packages l -> UTop.require l; true
          | `Object fn -> toploop_load_file Format.err_formatter fn)
        (List.rev !preload)
    in
    if ok then !Toploop.toplevel_startup_hook ();
    ok
  with exn ->
    try
      Errors.report_error Format.err_formatter exn;
      false
    with exn ->
      Format.eprintf "Uncaught exception: %s\n" (Printexc.to_string exn);
      false

external caml_sys_modify_argv : string array -> unit =
  "caml_sys_modify_argv"
let override_argv () =
  let len = Array.length Sys.argv - !Arg.current in
  let copy = Array.init len (fun i -> Sys.argv.(i+ !Arg.current)) in
  caml_sys_modify_argv copy;
  Arg.current := 0

let run_script name =
  (* To prevent message from camlp4 *)
  Sys.interactive := false;
  if not (prepare ()) then exit 2;
  override_argv ();
  Toploop.initialize_toplevel_env ();
  Location.input_name := UTop.input_name;
  if toploop_use_silently Format.err_formatter name then
    exit 0
  else
    exit 2

let file_argument name =
  if Filename.check_suffix name ".cmo" || Filename.check_suffix name ".cma" then
    preload := `Object name :: !preload
  else
    run_script name

let print_version () =
  Printf.printf "The universal toplevel for OCaml, version %s, compiled for OCaml version %s\n" UTop.version Sys.ocaml_version;
  exit 0

let print_version_num () =
  Printf.printf "%s\n" UTop.version;
  exit 0

(* Config from command line *)
let autoload = ref true

let args = Arg.align [
  "-absname", Arg.Set Clflags.absname, " Show absolute filenames in error message";
  "-I", Arg.String (fun dir ->  Clflags.include_dirs := dir :: !Clflags.include_dirs), "<dir> Add <dir> to the list of include directories";
  "-init", Arg.String (fun s -> Clflags.init_file := Some s), "<file> Load <file> instead of default init file";
  "-labels", Arg.Clear Clflags.classic, " Use commuting label mode";
  "-no-app-funct", Arg.Clear Clflags.applicative_functors, " Deactivate applicative functors";
  "-noassert", Arg.Set Clflags.noassert, " Do not compile assertion checks";
  "-nolabels", Arg.Set Clflags.classic, " Ignore non-optional labels in types";
  "-nostdlib", Arg.Set Clflags.no_std_include, " Do not add default directory to the list of include directories";
  "-ppx", Arg.String (fun ppx -> Clflags.all_ppx := ppx :: !Clflags.all_ppx), "<command> Pipe abstract syntax trees through preprocessor <command>";
  "-principal", Arg.Set Clflags.principal, " Check principality of type inference";
#if OCAML_VERSION < (5, 0, 0)
  "-safe-string", Arg.Clear Clflags.unsafe_string, " Make strings immutable";
#endif
  "-short-paths", Arg.Clear Clflags.real_paths, " Shorten paths in types (the default)";
  "-no-short-paths", Arg.Set Clflags.real_paths, " Do not shorten paths in types";
  "-rectypes", Arg.Set Clflags.recursive_types, " Allow arbitrary recursive types";
  "-stdin", Arg.Unit (fun () -> run_script ""), " Read script from standard input";
  "-strict-sequence", Arg.Set Clflags.strict_sequence, " Left-hand part of a sequence must have type unit";
  "-unsafe", Arg.Set Clflags.unsafe, " Do not compile bounds checking on array and string access";
  "-version", Arg.Unit print_version, " Print version and exit";
  "-vnum", Arg.Unit print_version_num, " Print version number and exit";
  "-w", Arg.String (fun opt -> ignore (Warnings.parse_options false opt)),
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
  "-warn-error", Arg.String (fun opt -> ignore (Warnings.parse_options true opt)),
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
  "-no-implicit-bindings", Arg.Unit (fun () -> UTop.set_create_implicits false),
  " Don't add implicit bindings for expressions (the default)";
  "-implicit-bindings", Arg.Unit (fun () -> UTop.set_create_implicits true),
  " Add implicit bindings: <expr>;; -> let _0 = <expr>;;";
  "-no-autoload", Arg.Clear autoload,
  " Disable autoloading of files in $OCAML_TOPLEVEL_PATH/autoload";
  "-require", Arg.String (fun s -> preload := `Packages (UTop.split_words s) :: !preload),
  "<package> Load this package";
  "-dparsetree", Arg.Set Clflags.dump_parsetree, " Dump OCaml AST after rewriting";
  "-dsource", Arg.Set Clflags.dump_source, " Dump OCaml source after rewriting";
]

let () = Clflags.real_paths := false

let app_name = Filename.basename Sys.executable_name
let usage = Printf.sprintf "Usage: %s <options> <object-files> [script-file [arguments]]\noptions are:" app_name

let load_init_files dir =
  let files = Sys.readdir dir in
  Array.sort String.compare files;
  Array.iter
    (fun fn ->
       if Filename.check_suffix fn ".ml" then
         ignore (toploop_use_silently Format.err_formatter (Filename.concat dir fn) : bool))
    files
;;

let common_init ~initial_env =
  (* Initializes toplevel environment. *)
  (match initial_env with
   | None -> Toploop.initialize_toplevel_env ()
   | Some env -> Toploop.toplevel_env := env);
  (* Set the global input name. *)
  Location.input_name := UTop.input_name;
  (* Make sure SIGINT is catched while executing OCaml code. *)
  Sys.catch_break true;
  (* Load system init files. *)
  (match try Some (Sys.getenv "OCAML_TOPLEVEL_PATH") with Not_found -> None with
    | Some dir ->
      Topdirs.dir_directory dir;
      let autoload_dir = Filename.concat dir "autoload" in
      if !autoload && !UTop_private.autoload && Sys.file_exists autoload_dir then
        load_init_files autoload_dir
    | None -> ());
  (* Load user's init file. *)
  let init_fn =
    match !Clflags.init_file with
     | Some fn ->
         if Sys.file_exists fn then
           Some fn
         else (
           Printf.eprintf "Init file not found: \"%s\".\n" fn;
           None
         )
     | None ->
         if Sys.file_exists ".ocamlinit" && Sys.getcwd () <> LTerm_resources.home then
           Some ".ocamlinit"
         else
           let xdg_fn = LTerm_resources.xdgbd_file ~loc:LTerm_resources.Config "utop/init.ml" in
           if Sys.file_exists xdg_fn then
             Some xdg_fn
           else
             let fn = Filename.concat LTerm_resources.home ".ocamlinit" in
             if Sys.file_exists fn then
               Some fn
             else
               None
  in
  (match init_fn with
   | None -> ()
   | Some fn ->
     ignore (toploop_use_silently Format.err_formatter fn : bool));
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
  Lwt.catch
    LTerm_inputrc.load
    (function
    | Unix.Unix_error (error, func, arg) ->
      Logs_lwt.err (fun m -> m "cannot load key bindings from %S: %s: %s" LTerm_inputrc.default func (Unix.error_message error))
    | LTerm_inputrc.Parse_error (fname, line, msg) ->
      Logs_lwt.err (fun m -> m "error in key bindings file %S, line %d: %s" fname line msg)
    | exn -> Lwt.fail exn)

let protocol_version = 1

let main_aux ~initial_env =
  Arg.parse args file_argument usage;
#if OCAML_VERSION >= (5, 0, 0) && OCAML_VERSION < (5, 1, 0)
  Topcommon.load_topdirs_signature ();
#endif
  if not (prepare ()) then exit 2;
  if !emacs_mode then begin
    Printf.printf "protocol-version:%d\n%!" protocol_version;
    UTop_private.set_ui UTop_private.Emacs;
    let module Emacs = Emacs (struct end) in
    Printf.printf "Welcome to utop version %s (using OCaml version %s)!\n\n%!" UTop.version Sys.ocaml_version;
    common_init ~initial_env;
    Emacs.loop ()
  end else begin
    UTop_private.set_ui UTop_private.Console;
    let term = Lwt_main.run (Lazy.force LTerm.stdout) in
    if LTerm.incoming_is_a_tty term && LTerm.outgoing_is_a_tty term then begin
      (* Set the initial size. *)
      UTop_private.set_size (S.const (LTerm.size term));
      (* Load user data. *)
      Lwt_main.run (Lwt.join [UTop_styles.load (); load_inputrc ()]);
      (* Display a welcome message. *)
      Lwt_main.run (welcome term);
      (* Common initialization. *)
      common_init ~initial_env;
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

let main_internal ~initial_env =
  try
    main_aux ~initial_env
  with exn ->
    (match exn with
#if OCAML_VERSION >= (4,12,0)
       | Compenv.Exit_with_status e -> exit e
#endif
       | Unix.Unix_error (error, func, "") ->
           Printf.eprintf "%s: %s: %s\n" app_name func (Unix.error_message error)
       | Unix.Unix_error (error, func, arg) ->
         Printf.eprintf "%s: %s(%S): %s\n" app_name func arg (Unix.error_message error)
       | exn ->
           Printf.eprintf "Fatal error: exception %s\n" (Printexc.to_string exn));
    Printexc.print_backtrace stderr;
    flush stderr;
    exit 2

let main () = main_internal ~initial_env:None

type value = V : string * _ -> value

exception Found of Env.t

let get_required_label name args =
  match List.find (fun (lab, _) -> lab = Asttypes.Labelled name) args with
  | _, x -> x
  | exception Not_found -> None

let walk dir ~init ~f =
  let rec loop dir acc =
    let acc = f dir acc in
    ArrayLabels.fold_left (Sys.readdir dir) ~init:acc ~f:(fun acc fn ->
      let fn = Filename.concat dir fn in
      match Unix.lstat fn with
      | { st_kind = S_DIR; _ } -> loop fn acc
      | _                      -> acc)
  in
  match Unix.lstat dir with
  | exception Unix.Unix_error(ENOENT, _, _) -> init
  | _ -> loop dir init

let iter_structure expr =
  let next iterator e = Tast_iterator.default_iterator.expr iterator e in
  let expr iterator = expr (next iterator) in
  let iter = { Tast_iterator.default_iterator with expr } in
  iter.structure iter

let interact ?(search_path=[]) ?(build_dir="_build") ~unit ~loc:(fname, lnum, cnum, _)
      ~values =
  let search_path = walk build_dir ~init:search_path ~f:(fun dir acc -> dir :: acc) in
  let cmt_fname =
    try
      UTop_compat.find_in_path_normalized
        search_path (unit ^ ".cmt")
    with Not_found ->
      Printf.ksprintf failwith "%s.cmt not found in search path!" unit
  in
  let cmt_infos = Cmt_format.read_cmt cmt_fname in
  let expr next (e : Typedtree.expression) =
    match e.exp_desc with
        | Texp_apply (_, args) -> begin
            try
              match get_required_label "loc"    args,
                    get_required_label "values" args
              with
              | Some l, Some v ->
                let pos = l.exp_loc.loc_start in
                if pos.pos_fname = fname &&
                   pos.pos_lnum = lnum   &&
                   pos.pos_cnum - pos.pos_bol = cnum then
                  raise (Found v.exp_env)
              | _ -> next e
            with Not_found -> next e
          end
        | _ -> next e
  in
  let search = iter_structure expr in
  try
    begin match cmt_infos.cmt_annots with
    | Implementation st -> search st
    | _ -> ()
    end;
    failwith "Couldn't find location in cmt file"
  with Found env ->
  try
    let visible_paths = UTop_compat.visible_paths_for_cmt_infos cmt_infos in
    List.iter Topdirs.dir_directory (search_path @ visible_paths);
    let env = Envaux.env_of_only_summary env in
    List.iter (fun (V (name, v)) -> Toploop.setvalue name (Obj.repr v)) values;
    main_internal ~initial_env:(Some env)
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2

let () =
  Location.register_error_of_exn
    (function
      | Envaux.Error err ->
        Some (Location.error_of_printer_file Envaux.report_error err)
      | _ -> None
    )
