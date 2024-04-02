(*
 * uTop_complete.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

[@@@warning "-9-27-32"]

open Types
open LTerm_read_line
open UTop_compat
open UTop_token

module String_set = Set.Make(String)
module String_map = Map.Make(String)

let set_of_list = List.fold_left (fun set x -> String_set.add x set) String_set.empty

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

(* Transform a non-empty list of strings into a long-identifier. *)
let longident_of_list = function
  | [] ->
      invalid_arg "UTop_complete.longident_of_list"
  | component :: rest ->
      let rec loop acc = function
        | [] -> acc
        | component :: rest -> loop (Longident.Ldot(acc, component)) rest
      in
      loop (Longident.Lident component) rest

(* Check whether an identifier is a valid one. *)
let is_valid_identifier id =
  id <> "" &&
    (match id.[0] with
       | 'A' .. 'Z' | 'a' .. 'z' |  '_' -> true
       | _ -> false)

let add id set = if is_valid_identifier id then String_set.add id set else set

let lookup_env f x env =
  try
    Some (f x env)
  with Not_found | Env.Error _ ->
    None

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

(* The following functions takes a list of tokens in reverse order. *)

type value_or_field = Value | Field
    (* Either a value, or a record field. *)

(* Parse something of the form [M1.M2. ... .Mn.id] or
   [field.M1.M2. ... .Mn.id] *)
let parse_longident tokens =
  let rec loop acc tokens =
    match tokens with
      | (Symbol ".", _) :: (Uident id, _) :: tokens ->
          loop (id :: acc) tokens
      | (Symbol ".", _) :: (Lident id, _) :: tokens ->
          (Field,
           match acc with
             | [] -> None
             | l -> Some (longident_of_list l))
      | _ ->
          (Value,
           match acc with
             | [] -> None
             | l -> Some (longident_of_list l))
  in
  match tokens with
    | ((Comment (_, false) | String (_, false) | Quotation (_, false)), _) :: _ ->
        (* An unterminated command, string, or quotation. *)
        None
    | ((Uident id | Lident id), { idx1 = start }) :: tokens ->
        (* An identifier. *)
        let kind, path = loop [] tokens in
        Some (kind, path, start, id)
    | (Blanks, { idx2 = stop }) :: tokens ->
        (* Some blanks at the end. *)
        let kind, path = loop [] tokens in
        Some (kind, path, stop, "")
    | (_, { idx2 = stop }) :: _ ->
        (* Otherwise complete after the last token. *)
        let kind, path = loop [] tokens in
        Some (kind, path, stop, "")
    | [] ->
        None

(* Parse something of the form [M1.M2. ... .Mn.id#m1#m2# ... #mp#m] *)
let parse_method tokens =
  (* Collect [M1.M2. ... .Mn.id] and returns the corresponding
     longidentifier. *)
  let rec loop_uidents acc tokens =
    match tokens with
      | (Symbol ".", _) :: (Uident id, _) :: tokens ->
          loop_uidents (id :: acc) tokens
      | _ ->
          longident_of_list acc
  in
  (* Collect [m1#m2# ... #mp] *)
  let rec loop_methods acc tokens =
    match tokens with
      | (Lident meth, _) :: (Symbol "#", _) :: tokens ->
          loop_methods (meth :: acc) tokens
      | (Lident id, _) :: tokens ->
          Some (loop_uidents [id] tokens, acc)
      | _ ->
          None
  in
  match tokens with
    | (Lident meth, { idx1 = start }) :: (Symbol "#", _) :: tokens -> begin
        match loop_methods [] tokens with
          | None -> None
          | Some (path, meths) -> Some (path, meths, start, meth)
      end
    | (Symbol "#", { idx2 = stop }) :: tokens
    | (Blanks, { idx2 = stop }) :: (Symbol "#", _) :: tokens -> begin
        match loop_methods [] tokens with
          | None -> None
          | Some (path, meths) -> Some (path, meths, stop, "")
      end
    | _ ->
        None

type label_kind = Required | Optional
    (* Kind of labels: required or optional. *)

type fun_or_new = Fun | New
    (* Either a function application, either an object creation. *)

(* Parse something of the form [M1.M2. ... .Mn.id#m1#m2# ... #mp expr1 ... exprq ~label]
   or [new M1.M2. ... .Mn.id expr1 ... exprq ~label] *)
let parse_label tokens =
  (* Collect [M1.M2. ... .Mn] *)
  let rec loop_uidents acc_uidents acc_methods tokens =
    match tokens with
      | (Lident "new", _) :: _ ->
          Some (New, longident_of_list acc_uidents, acc_methods)
      | ((Lident id | Uident id), _) :: _ when String_set.mem id !UTop.keywords ->
          Some (Fun, longident_of_list acc_uidents, acc_methods)
      | (Symbol ".", _) :: (Uident id, _) :: tokens ->
          loop_uidents (id :: acc_uidents) acc_methods tokens
      | (Symbol ("~" | "?" | ":" | "." | "#" | "!" | "`"), _) :: tokens ->
          search tokens
      | (Symbol ")", _) :: tokens ->
          skip tokens "(" []
      | (Symbol "}", _) :: tokens ->
          skip tokens "{" []
      | (Symbol "]", _) :: tokens ->
          skip tokens "[" []
      | (Symbol _, _) :: _ ->
          Some (Fun, longident_of_list acc_uidents, acc_methods)
      | [] ->
          Some (Fun, longident_of_list acc_uidents, acc_methods)
      | _ ->
          search tokens
  and loop_methods acc tokens =
    match tokens with
      | ((Lident id | Uident id), _) :: _ when String_set.mem id !UTop.keywords ->
          None
      | (Symbol ("~" | "?" | ":" | "." | "#" | "!" | "`"), _) :: tokens ->
          search tokens
      | (Symbol ")", _) :: tokens ->
          skip tokens "(" []
      | (Symbol "}", _) :: tokens ->
          skip tokens "{" []
      | (Symbol "]", _) :: tokens ->
          skip tokens "[" []
      | (Symbol _, _) :: _ ->
          None
      | (Lident id, _) :: (Symbol "#", _) :: tokens ->
          loop_methods (id :: acc) tokens
      | (Lident id, _) :: tokens ->
          loop_uidents [id] acc tokens
      | [] ->
          None
      | _ ->
          search tokens
  and search tokens =
    match tokens with
      | ((Lident id | Uident id), _) :: _ when String_set.mem id !UTop.keywords ->
          None
      | (Symbol ("~" | "?" | ":" | "." | "#" | "!" | "`"), _) :: tokens ->
          search tokens
      | (Symbol ")", _) :: tokens ->
          skip tokens "(" []
      | (Symbol "}", _) :: tokens ->
          skip tokens "{" []
      | (Symbol "]", _) :: tokens ->
          skip tokens "[" []
      | (Symbol _, _) :: _ ->
          None
      | (Lident id, _) :: (Symbol "#", _) :: tokens ->
          loop_methods [id] tokens
      | (Lident id, _) :: tokens ->
          loop_uidents [id] [] tokens
      | _ :: tokens ->
          search tokens
      | [] ->
          None
  and skip tokens top stack =
    match tokens with
      | (Symbol symbol, _) :: tokens when symbol = top -> begin
          match stack with
            | [] -> search tokens
            | top :: stack -> skip tokens top stack
        end
      | (Symbol ")", _) :: tokens ->
          skip tokens "(" (top :: stack)
      | (Symbol "}", _) :: tokens ->
          skip tokens "{" (top :: stack)
      | (Symbol "]", _) :: tokens ->
          skip tokens "[" (top :: stack)
      | _ :: tokens ->
          skip tokens top stack
      | [] ->
          None
  in
  match tokens with
    | (Lident label, { idx1 = start }) :: (Symbol "~", _) :: tokens -> begin
        match search tokens with
          | None -> None
          | Some (kind, id, meths) -> Some (kind, id, meths, Required, start, label)
      end
    | (Symbol "~", { idx2 = stop }) :: tokens -> begin
        match search tokens with
          | None -> None
          | Some (kind, id, meths) -> Some (kind, id, meths, Required, stop, "")
      end
    | (Lident label, { idx1 = start }) :: (Symbol "?", _) :: tokens -> begin
        match search tokens with
          | None -> None
          | Some (kind, id, meths) -> Some (kind, id, meths, Optional, start, label)
      end
    | (Symbol "?", { idx2 = stop }) :: tokens -> begin
        match search tokens with
          | None -> None
          | Some (kind, id, meths) -> Some (kind, id, meths, Optional, stop, "")
      end
    | _ ->
        None

(* +-----------------------------------------------------------------+
   | Directive listing                                               |
   +-----------------------------------------------------------------+ *)

let list_directives phrase_terminator =
  String_map.bindings
    (List.fold_left
       (fun map dir ->
          let suffix =
            match toploop_get_directive dir with
              | Some (Toploop.Directive_none _) -> phrase_terminator
              | Some (Toploop.Directive_string _) -> " \""
              | Some (Toploop.Directive_bool _  | Toploop.Directive_int _ | Toploop.Directive_ident _) -> " "
              | None -> assert false
          in
          String_map.add dir suffix map)
       String_map.empty
       (toploop_all_directive_names ()))

(* +-----------------------------------------------------------------+
   | File listing                                                    |
   +-----------------------------------------------------------------+ *)

type file_kind = Directory | File

let basename name =
  let name' = Filename.basename name in
  if name' = "." && not (Zed_utf8.ends_with name ".") then
    ""
  else
    name'

let add_files filter acc dir =
  Array.fold_left
    (fun map name ->
       let absolute_name = Filename.concat dir name in
       if try Sys.is_directory absolute_name with Sys_error _ -> false then
         String_map.add (Filename.concat name "") Directory map
       else if filter name then
         String_map.add name File map
       else
         map)
    acc
    (try Sys.readdir dir with Sys_error _ -> [||])

let list_directories dir =
  String_set.elements
    (Array.fold_left
       (fun set name ->
          let absolute_name = Filename.concat dir name in
          if try Sys.is_directory absolute_name with Sys_error _ -> false then
            String_set.add name set
          else
            set)
       String_set.empty
       (try Sys.readdir (if dir = "" then Filename.current_dir_name else dir) with Sys_error _ -> [||]))

let path () =
  let path_separator =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> ':'
    | "Win32" -> ';'
    | _ -> assert false in
  let split str sep =
    let rec split_rec pos =
      if pos >= String.length str then [] else begin
        match try  Some (String.index_from str pos sep)
              with Not_found -> None with
        | Some newpos ->
          String.sub str pos (newpos - pos) ::
          split_rec (newpos + 1)
        | None ->
          [String.sub str pos (String.length str - pos)]
      end in
    split_rec 0
  in
  try
    split (Sys.getenv "PATH") path_separator
  with Not_found -> []

(* +-----------------------------------------------------------------+
   | Names listing                                                   |
   +-----------------------------------------------------------------+ *)

module Path_map = Map.Make(struct type t = Path.t let compare = compare end)
module Longident_map = Map.Make(struct type t = Longident.t let compare = compare end)

(* All names accessible without a path. *)
let global_names = ref None
let global_names_revised = ref None

(* All names accessible with a path, by path. *)
let local_names_by_path = ref Path_map.empty

(* All names accessible with a path, by long identifier. *)
let local_names_by_longident = ref Longident_map.empty

(* All record fields accessible without a path. *)
let global_fields = ref None

(* All record fields accessible with a path, by path. *)
let local_fields_by_path = ref Path_map.empty

(* All record fields accessible with a path, by long identifier. *)
let local_fields_by_longident = ref Longident_map.empty

(* All visible modules according to Config.load_path. *)
let visible_modules = ref None

let reset () =
  visible_modules := None;
  global_names := None;
  global_names_revised := None;
  local_names_by_path := Path_map.empty;
  local_names_by_longident := Longident_map.empty;
  global_fields := None;
  local_fields_by_path := Path_map.empty;
  local_fields_by_longident := Longident_map.empty

let get_cached var f =
  match !var with
  | Some x ->
    x
  | None ->
    let x = f () in
    var := Some x;
    x

(* List all visible modules. *)
let visible_modules () =
  get_cached visible_modules
    (fun () ->
      List.fold_left
        (fun acc dir ->
          try
            Array.fold_left
              (fun acc fname ->
                if Filename.check_suffix fname ".cmi" then
                  String_set.add (String.capitalize_ascii (Filename.chop_suffix fname ".cmi")) acc
                else
                  acc)
              acc
              (Sys.readdir (if dir = "" then Filename.current_dir_name else dir))
          with Sys_error _ ->
            acc)
        String_set.empty @@ UTop_compat.get_load_path ()
    )

let field_name { ld_id = id } = Ident.name id
let constructor_name { cd_id = id } = Ident.name id

let add_fields_of_type decl acc =
  match decl.type_kind with
    | Type_variant _ ->
        acc
    | Type_record (fields, _) ->
        List.fold_left (fun acc field -> add (field_name field) acc) acc fields
#if OCAML_VERSION >= (5, 2, 0)
    | Type_abstract _ ->
#else 
    | Type_abstract ->
#endif
        acc
    | Type_open ->
        acc

let add_names_of_type decl acc =
  match decl.type_kind with
#if OCAML_VERSION >= (4, 13, 0)
    | Type_variant (constructors, _) ->
#else
    | Type_variant constructors ->
#endif
        List.fold_left (fun acc cstr -> add (constructor_name cstr) acc) acc constructors
    | Type_record (fields, _) ->
        List.fold_left (fun acc field -> add (field_name field) acc) acc fields
#if OCAML_VERSION >= (5, 2, 0)
    | Type_abstract _ ->
#else 
    | Type_abstract ->
#endif
        acc
    | Type_open ->
        acc

let path_of_mty_alias = function
  | Mty_alias path -> path
  | _ -> assert false

let rec names_of_module_type = function
  | Mty_signature decls ->
      List.fold_left
        (fun acc decl -> match decl with
           | Sig_value (id, _, _)
           | Sig_typext (id, _, _, _)
           | Sig_module (id, _, _, _, _)
           | Sig_modtype (id, _, _)
           | Sig_class (id, _, _, _)
           | Sig_class_type (id, _, _, _) ->
               add (Ident.name id) acc
           | Sig_type (id, decl, _, _) ->
               add_names_of_type decl (add (Ident.name id) acc))
        String_set.empty decls
  | Mty_ident path -> begin
      match lookup_env Env.find_modtype path !Toploop.toplevel_env with
        | Some { mtd_type = None } -> String_set.empty
        | Some { mtd_type = Some module_type } -> names_of_module_type module_type
        | None -> String_set.empty
    end
  | Mty_alias _ as mty_alias -> begin
      let path = path_of_mty_alias mty_alias in
      match lookup_env Env.find_module path !Toploop.toplevel_env with
        | None -> String_set.empty
        | Some { md_type = module_type } -> names_of_module_type module_type
    end
  | _ ->
      String_set.empty

let rec fields_of_module_type = function
  | Mty_signature decls ->
      List.fold_left
        (fun acc decl -> match decl with
           | Sig_value _
           | Sig_typext _
           | Sig_module _
           | Sig_modtype _
           | Sig_class _
           | Sig_class_type _ ->
               acc
           | Sig_type (_, decl, _, _) ->
               add_fields_of_type decl acc)
        String_set.empty decls
  | Mty_ident path -> begin
      match lookup_env Env.find_modtype path !Toploop.toplevel_env with
        | Some { mtd_type = None } -> String_set.empty
        | Some { mtd_type = Some module_type } -> fields_of_module_type module_type
        | None -> String_set.empty
    end
  | Mty_alias _ as mty_alias -> begin
      let path = path_of_mty_alias mty_alias in
      match lookup_env Env.find_module path !Toploop.toplevel_env with
        | None -> String_set.empty
        | Some { md_type = module_type } -> fields_of_module_type module_type
  end
  | _ ->
      String_set.empty

let find_module path env = (Env.find_module path env).md_type

let names_of_module longident =
  try
    Longident_map.find longident !local_names_by_longident
  with Not_found ->
    match lookup_env Env.find_module_by_name longident !Toploop.toplevel_env with
      | Some(path, {md_type; _}) ->
          let names = names_of_module_type md_type in
          local_names_by_path := Path_map.add path names !local_names_by_path;
          local_names_by_longident := Longident_map.add longident names !local_names_by_longident;
          names
      | None ->
          local_names_by_longident := Longident_map.add longident String_set.empty !local_names_by_longident;
          String_set.empty

let fields_of_module longident =
  try
    Longident_map.find longident !local_fields_by_longident
  with Not_found ->
    match lookup_env Env.find_module_by_name longident !Toploop.toplevel_env with
      | Some(path, {md_type; _}) ->
          let fields = fields_of_module_type md_type in
          local_fields_by_path := Path_map.add path fields !local_fields_by_path;
          local_fields_by_longident := Longident_map.add longident fields !local_fields_by_longident;
          fields
      | None ->
          local_fields_by_longident := Longident_map.add longident String_set.empty !local_fields_by_longident;
          String_set.empty

let list_global_names () =
  let rec loop acc = function
    | Env.Env_empty -> acc
    | Env.Env_value_unbound _-> acc
    | Env.Env_module_unbound _-> acc
    | Env.Env_value(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_type(summary, id, decl) ->
        loop (add_names_of_type decl (add (Ident.name id) acc)) summary
    | Env.Env_extension(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_module(summary, id, _, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_modtype(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_class(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_cltype(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_functor_arg(summary, id) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_persistent (summary, id) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_constraints (summary, _) ->
        loop acc summary
    | Env.Env_copy_types summary ->
        loop acc summary
    | Env.Env_open(summary, path) ->
        match try Some (Path_map.find path !local_names_by_path) with Not_found -> None with
          | Some names ->
              loop (String_set.union acc names) summary
          | None ->
              match lookup_env find_module path !Toploop.toplevel_env with
                | Some module_type ->
                    let names = names_of_module_type module_type in
                    local_names_by_path := Path_map.add path names !local_names_by_path;
                    loop (String_set.union acc names) summary
                | None ->
                    local_names_by_path := Path_map.add path String_set.empty !local_names_by_path;
                    loop acc summary
  in
  (* Add names of the environment: *)
  let acc = loop String_set.empty (Env.summary !Toploop.toplevel_env) in
  (* Add accessible modules: *)
  String_set.union acc (visible_modules ())

let global_names () = get_cached global_names list_global_names

let replace x y set =
  if String_set.mem x set then
    String_set.add y (String_set.remove x set)
  else
    set

let list_global_fields () =
  let rec loop acc = function
    | Env.Env_empty -> acc
    | Env.Env_value_unbound _-> acc
    | Env.Env_module_unbound _-> acc
    | Env.Env_value(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_type(summary, id, decl) ->
        loop (add_fields_of_type decl (add (Ident.name id) acc)) summary
    | Env.Env_extension(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_module(summary, id, _, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_functor_arg(summary, id) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_modtype(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_class(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_cltype(summary, id, _) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_persistent (summary, id) ->
        loop (add (Ident.name id) acc) summary
    | Env.Env_constraints (summary, _) ->
        loop acc summary
    | Env.Env_copy_types summary ->
        loop acc summary
    | Env.Env_open(summary, path) ->
        match try Some (Path_map.find path !local_fields_by_path) with Not_found -> None with
          | Some fields ->
              loop (String_set.union acc fields) summary
          | None ->
              match lookup_env find_module path !Toploop.toplevel_env with
                | Some module_type ->
                    let fields = fields_of_module_type module_type in
                    local_fields_by_path := Path_map.add path fields !local_fields_by_path;
                    loop (String_set.union acc fields) summary
                | None ->
                    local_fields_by_path := Path_map.add path String_set.empty !local_fields_by_path;
                    loop acc summary
  in
  (* Add fields of the environment: *)
  let acc = loop String_set.empty (Env.summary !Toploop.toplevel_env) in
  (* Add accessible modules: *)
  String_set.union acc (visible_modules ())

let global_fields () = get_cached global_fields list_global_fields

(* +-----------------------------------------------------------------+
   | Listing methods                                                 |
   +-----------------------------------------------------------------+ *)

let rec find_method meth type_expr =
  match get_desc type_expr with
    | Tlink type_expr ->
        find_method meth type_expr
    | Tobject (type_expr, _) ->
        find_method meth type_expr
    | Tfield (name, _, type_expr, rest) ->
        if name = meth then
          Some type_expr
        else
          find_method meth rest
    | Tpoly (type_expr, _) ->
        find_method meth type_expr
    | Tconstr (path, _, _) -> begin
        match lookup_env Env.find_type path !Toploop.toplevel_env with
          | None
          | Some { type_manifest = None } ->
              None
          | Some { type_manifest = Some type_expr } ->
              find_method meth type_expr
      end
    | _ ->
        None

let rec methods_of_type acc type_expr =
  match get_desc type_expr with
    | Tlink type_expr ->
        methods_of_type acc type_expr
    | Tobject (type_expr, _) ->
        methods_of_type acc type_expr
    | Tfield (name, _, _, rest) ->
        methods_of_type (add name acc) rest
    | Tpoly (type_expr, _) ->
        methods_of_type acc type_expr
    | Tconstr (path, _, _) -> begin
        match lookup_env Env.find_type path !Toploop.toplevel_env with
          | None
          | Some { type_manifest = None } ->
              acc
          | Some { type_manifest = Some type_expr } ->
              methods_of_type acc type_expr
      end
    | _ ->
        acc

let rec find_object meths type_expr =
  match meths with
    | [] ->
        Some type_expr
    | meth :: meths ->
        match find_method meth type_expr with
          | Some type_expr ->
              find_object meths type_expr
          | None ->
              None

let methods_of_object longident meths =
  match lookup_env Env.find_value_by_name longident !Toploop.toplevel_env with
    | None ->
        []
    | Some (path, { val_type = type_expr }) ->
        match find_object meths type_expr with
          | None ->
              []
          | Some type_expr ->
              String_set.elements (methods_of_type String_set.empty type_expr)

(* +-----------------------------------------------------------------+
   | Listing labels                                                  |
   +-----------------------------------------------------------------+ *)

let rec labels_of_type acc type_expr =
  match get_desc type_expr with
    | Tlink te ->
        labels_of_type acc te
    | Tpoly (te, _) ->
        labels_of_type acc te
    | Tarrow(label, _, te, _) ->
      (match label with
      | Nolabel ->
        labels_of_type acc te
      | Optional label ->
        labels_of_type (String_map.add label Optional acc) te
      | Labelled label ->
        labels_of_type (String_map.add label Required acc) te)
    | Tconstr(path, _, _) -> begin
        match lookup_env Env.find_type path !Toploop.toplevel_env with
          | None
          | Some { type_manifest = None } ->
              String_map.bindings acc
          | Some { type_manifest = Some type_expr } ->
              labels_of_type acc type_expr
      end
    | _ ->
        String_map.bindings acc

let labels_of_function longident meths =
  match lookup_env Env.find_value_by_name longident !Toploop.toplevel_env with
    | None ->
        []
    | Some (path, { val_type = type_expr }) ->
        match find_object meths type_expr with
          | None ->
              []
          | Some type_expr ->
              labels_of_type String_map.empty type_expr

let labels_of_newclass longident =
  match lookup_env Env.find_class_by_name longident !Toploop.toplevel_env with
    | None ->
        []
    | Some (path, { cty_new = None }) ->
        []
    | Some (path, { cty_new = Some type_expr }) ->
        labels_of_type String_map.empty type_expr

(* +-----------------------------------------------------------------+
   | Tokens processing                                               |
   +-----------------------------------------------------------------+ *)

(* Filter blanks and comments except for the last token. *)
let filter tokens =
  let rec aux acc = function
    | [] -> acc
    | [((Blanks | Comment (_, true)), loc)] -> (Blanks, loc) :: acc
    | ((Blanks | Comment (_, true)), _) :: rest -> aux acc rest
    | x :: rest -> aux (x :: acc) rest
  in
  List.rev (aux [] tokens)

(* Reverse and filter blanks and comments except for the last
   token. *)
let rec rev_filter acc tokens =
  match tokens with
    | [] -> acc
    | [((Blanks | Comment (_, true)), loc)] -> (Blanks, loc) :: acc
    | ((Blanks | Comment (_, true)), _) :: rest -> rev_filter acc rest
    | x :: rest -> rev_filter (x :: acc) rest

(* Find the current context. *)
let rec find_context tokens = function
  | [] ->
      Some (rev_filter [] tokens)
  | [(Quotation (items, false), _)] ->
      find_context_in_quotation items
  | _ :: rest ->
      find_context tokens rest

and find_context_in_quotation = function
  | [] ->
      None
  | [(Quot_anti { a_closing = None; a_contents = tokens }, _)] ->
      find_context tokens tokens
  | _ :: rest ->
      find_context_in_quotation rest

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

let complete ~phrase_terminator ~input =
  let true_name, false_name = ("true", "false") in
  let tokens = UTop_lexer.lex_string input in
  (* Filter blanks and comments. *)
  let tokens = filter tokens in
  match tokens with

    (* Completion on directive names. *)
    | [(Symbol "#", { idx2 = stop })]
    | [(Symbol "#", _); (Blanks, { idx2 = stop })] ->
        (stop, list_directives phrase_terminator)
    | [(Symbol "#", _); ((Lident src | Uident src), { idx1 = start })] ->
        (start, lookup_assoc src (list_directives phrase_terminator))

    (* Complete with ";;" when possible. *)
    | [(Symbol "#", _); ((Lident _ | Uident _), _); (String (_, true), { idx2 = stop })]
    | [(Symbol "#", _); ((Lident _ | Uident _), _); (String (_, true), _); (Blanks, { idx2 = stop })] ->
        (stop, [(phrase_terminator, "")])
    | [(Symbol "#", _); ((Lident _ | Uident _), _); (String (_, true), _); (Symbol sym, { idx1 = start })] ->
        if Zed_utf8.starts_with phrase_terminator sym then
          (start, [(phrase_terminator, "")])
        else
          (0, [])

    (* Completion on #require. *)
    | [(Symbol "#", _); (Lident "require", _); (String (tlen, false), loc)] ->
        let pkg = String.sub input (loc.ofs1 + tlen) (String.length input - loc.ofs1 - tlen) in
        let pkgs = lookup pkg (Fl_package_base.list_packages ()) in
        (loc.idx1 + 1, List.map (fun pkg -> (pkg, "\"" ^ phrase_terminator)) (List.sort compare pkgs))

    | [(Symbol "#", _); (Lident "typeof", _); (String (tlen, false), loc)] ->
      let prefix = String.sub input (loc.ofs1 + tlen) (String.length input - loc.ofs1 - tlen) in
      begin match Parse.longident (Lexing.from_string prefix) with
      | Longident.Ldot (lident, last_prefix) ->
        let set = names_of_module lident in
        let compls = lookup last_prefix (String_set.elements set) in
        let start = loc.idx1 + 1 + (String.length prefix - String.length last_prefix) in
        (start, List.map (fun w -> (w, "")) compls)
      | _ ->
        let set = global_names () in
        let compls = lookup prefix (String_set.elements set) in
        (loc.idx1 + 1, List.map (fun w -> (w, "")) compls)
      end

    (* Completion on #load. *)
    | [(Symbol "#", _); (Lident ("load" | "load_rec"), _); (String (tlen, false), loc)] ->
        let file = String.sub input (loc.ofs1 + tlen) (String.length input - loc.ofs1 - tlen) in
        let filter name = Filename.check_suffix name ".cma" || Filename.check_suffix name ".cmo" in
        let map =
          if Filename.is_relative file then
            let dir = Filename.dirname file in
            List.fold_left
              (fun acc d -> add_files filter acc (Filename.concat d dir))
              String_map.empty
              (Filename.current_dir_name ::
                (UTop_compat.get_load_path ())
              )

          else
            add_files filter String_map.empty (Filename.dirname file)
        in
        let list = String_map.bindings map in
        let name = basename file in
        let result = lookup_assoc name list in
        (loc.idx2 - Zed_utf8.length name,
         List.map (function (w, Directory) -> (w, "") | (w, File) -> (w, "\"" ^ phrase_terminator)) result)

    (* Completion on #ppx. *)
    | [(Symbol "#", _); (Lident ("ppx"), _); (String (tlen, false), loc)] ->
        let file = String.sub input (loc.ofs1 + tlen) (String.length input - loc.ofs1 - tlen) in
        let filter ~dir_ok name =
          try
            Unix.access name [Unix.X_OK];
            let kind     = (Unix.stat name).Unix.st_kind in
            let basename = Filename.basename name in
            (kind = Unix.S_REG && String.length basename >= 4 &&
                String.sub basename 0 4 = "ppx_") ||
              (dir_ok && kind = Unix.S_DIR)
          with Unix.Unix_error _ -> false
        in
        let map =
          if Filename.dirname file = "." && not (Filename.is_implicit file) then
            let dir = Filename.dirname file in
            add_files (filter ~dir_ok:true) String_map.empty dir
          else
            List.fold_left
              (fun acc dir -> add_files (fun name ->
                  filter ~dir_ok:false (Filename.concat dir name)) acc dir)
              String_map.empty (path ())
        in
        let list = String_map.bindings map in
        let name = basename file in
        let result = lookup_assoc name list in
        (loc.idx2 - Zed_utf8.length name,
         List.map (function (w, Directory) -> (w, "") | (w, File) -> (w, "\"" ^ phrase_terminator)) result)

    (* Completion on #use and #mod_use *)
    | [(Symbol "#", _); (Lident "use", _); (String (tlen, false), loc)]
    | [(Symbol "#", _); (Lident "mod_use", _); (String (tlen, false), loc)] ->
        let file = String.sub input (loc.ofs1 + tlen) (String.length input - loc.ofs1 - tlen) in
        let filter name =
          match try Some (String.rindex name '.') with Not_found -> None with
            | None ->
                true
            | Some idx ->
                let ext = String.sub name (idx + 1) (String.length name - (idx + 1)) in
                ext = "ml"
        in
        let map =
          if Filename.is_relative file then
            let dir = Filename.dirname file in
            List.fold_left
              (fun acc d -> add_files filter acc (Filename.concat d dir))
              String_map.empty
              (Filename.current_dir_name ::
                (UTop_compat.get_load_path ())
              )
          else
            add_files filter String_map.empty (Filename.dirname file)
        in
        let list = String_map.bindings map in
        let name = basename file in
        let result = lookup_assoc name list in
        (loc.idx2 - Zed_utf8.length name,
         List.map (function (w, Directory) -> (w, "") | (w, File) -> (w, "\"" ^ phrase_terminator)) result)

    (* Completion on #directory and #cd. *)
    | [(Symbol "#", _); (Lident ("cd" | "directory"), _); (String (tlen, false), loc)] ->
        let file = String.sub input (loc.ofs1 + tlen) (String.length input - loc.ofs1 - tlen) in
        let list = list_directories (Filename.dirname file) in
        let name = basename file in
        let result = lookup name list in
        (loc.idx2 - Zed_utf8.length name, List.map (function dir -> (dir, "")) result)

    (* Generic completion on directives. *)
    | [(Symbol "#", _); ((Lident dir | Uident dir), _); (Blanks, { idx2 = stop })] ->
        (stop,
         match toploop_get_directive dir with
           | Some (Toploop.Directive_none _) -> [(phrase_terminator, "")]
           | Some (Toploop.Directive_string _) -> [(" \"", "")]
           | Some (Toploop.Directive_bool _) -> [(true_name, phrase_terminator); (false_name, phrase_terminator)]
           | Some (Toploop.Directive_int _) -> []
           | Some (Toploop.Directive_ident _) -> List.map (fun w -> (w, "")) (String_set.elements (global_names ()))
           | None -> [])
    | (Symbol "#", _) :: ((Lident dir | Uident dir), _) :: tokens -> begin
        match toploop_get_directive dir with
          | Some (Toploop.Directive_none _) ->
              (0, [])
          | Some (Toploop.Directive_string _) ->
              (0, [])
          | Some (Toploop.Directive_bool _) -> begin
              match tokens with
                | [(Lident id, { idx1 = start })] ->
                    (start, lookup_assoc id [(true_name, phrase_terminator); (false_name, phrase_terminator)])
                | _ ->
                    (0, [])
            end
          | Some (Toploop.Directive_int _) ->
              (0, [])
          | Some (Toploop.Directive_ident _) -> begin
              match parse_longident (List.rev tokens) with
                | Some (Value, None, start, id) ->
                    (start, List.map (fun w -> (w, "")) (lookup id (String_set.elements (global_names ()))))
                | Some (Value, Some longident, start, id) ->
                    (start, List.map (fun w -> (w, "")) (lookup id (String_set.elements (names_of_module longident))))
                | _ ->
                    (0, [])
            end
          | None ->
              (0, [])
      end

    (* Completion on identifiers. *)
    | _ ->
        match find_context tokens tokens with
          | None ->
              (0, [])
          | Some [] ->
              (0, List.map (fun w -> (w, "")) (String_set.elements (String_set.union !UTop.keywords (global_names ()))))
          | Some tokens ->
              match parse_method tokens with
                | Some (longident, meths, start, meth) ->
                    (start, List.map (fun w -> (w, "")) (lookup meth (methods_of_object longident meths)))
                | None ->
                    match parse_label tokens with
                      | Some (Fun, longident, meths, Optional, start, label) ->
                          (start, List.map (fun (w, kind) -> (w, ":")) (lookup_assoc label (List.filter (function (w, Optional) -> true | (w, Required) -> false) (labels_of_function longident meths))))
                      | Some (Fun, longident, meths, Required, start, label) ->
                          (start, List.map (fun (w, kind) -> (w, ":")) (lookup_assoc label (labels_of_function longident meths)))
                      | Some (New, longident, meths, Optional, start, label) ->
                          (start, List.map (fun (w, kind) -> (w, ":")) (lookup_assoc label (List.filter (function (w, Optional) -> true | (w, Required) -> false) (labels_of_newclass longident))))
                      | Some (New, longident, meths, Required, start, label) ->
                          (start, List.map (fun (w, kind) -> (w, ":")) (lookup_assoc label (labels_of_newclass longident)))
                      | None ->
                          match parse_longident tokens with
                            | None ->
                                (0, [])
                            | Some (Value, None, start, id) ->
                                (start, List.map (fun w -> (w, "")) (lookup id (String_set.elements (String_set.union !UTop.keywords (global_names ())))))
                            | Some (Value, Some longident, start, id) ->
                                (start, List.map (fun w -> (w, "")) (lookup id (String_set.elements (names_of_module longident))))
                            | Some (Field, None, start, id) ->
                                (start, List.map (fun w -> (w, "")) (lookup id (String_set.elements (global_fields ()))))
                            | Some (Field, Some longident, start, id) ->
                                (start, List.map (fun w -> (w, "")) (lookup id (String_set.elements (fields_of_module longident))))

let complete ~phrase_terminator ~input =
  try
    (complete ~phrase_terminator ~input : int * (string * string) list)
  with Cmi_format.Error _ ->
    (0, [])
