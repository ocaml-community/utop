(*
 * uTop_complete.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open Types
open LTerm_read_line
open UTop_token

module String_set = Set.Make(String)
module String_map = Map.Make(String)

let set_of_list = List.fold_left (fun set x -> String_set.add x set) String_set.empty

(* +-----------------------------------------------------------------+
   | Directives                                                      |
   +-----------------------------------------------------------------+ *)

let get_directives () =
  String_map.bindings
    (Hashtbl.fold
       (fun dir kind map ->
          let suffix =
            match kind with
              | Toploop.Directive_none _ -> ";;"
              | Toploop.Directive_string _ -> " \""
              | Toploop.Directive_bool _  | Toploop.Directive_int _ | Toploop.Directive_ident _ -> " "
          in
          String_map.add dir suffix map)
       Toploop.directive_table
       String_map.empty)

(* +-----------------------------------------------------------------+
   | Files                                                           |
   +-----------------------------------------------------------------+ *)

type file_kind = Directory | File

let basename name =
  let name' = Filename.basename name in
  if name' = "." && not (Zed_utf8.ends_with name ".") then
    ""
  else
    name'

let list_files filter dir =
  String_map.bindings
    (Array.fold_left
       (fun map name ->
          let absolute_name = Filename.concat dir name in
          if try Sys.is_directory absolute_name with _ -> false then
            String_map.add (Filename.concat name "") Directory map
          else if filter name then
            String_map.add name File map
          else
            map)
       String_map.empty
       (Sys.readdir (if dir = "" then Filename.current_dir_name else dir)))

let list_directories dir =
  String_set.elements
    (Array.fold_left
       (fun set name ->
          let absolute_name = Filename.concat dir name in
          if try Sys.is_directory absolute_name with _ -> false then
            String_set.add name set
          else
            set)
       String_set.empty
       (Sys.readdir (if dir = "" then Filename.current_dir_name else dir)))

(* +-----------------------------------------------------------------+
   | Identifiers                                                     |
   +-----------------------------------------------------------------+ *)

let rec get_ident acc tokens =
  match tokens with
    | [(Uident, _, _, id); (Symbol, _, stop, ".")]
    | [(Uident, _, _, id); (Symbol, _, _, "."); (Blanks, _, stop, _)] ->
        Some (List.rev (id :: acc) , "", stop)
    | (Uident, _, _, id) :: (Symbol, _, _, ".") :: rest ->
        get_ident (id :: acc) rest
    | [((Uident | Lident), start, _, id)] ->
        Some (List.rev acc, id, start)
    | [((Comment false | Doc false | String false | Quotation false), _, _, _)] ->
        None
    | [(_, _, stop, _)] ->
        Some ([], "", stop)
    | [] ->
        None
    | _  :: rest ->
        get_ident [] rest

type path =
  | Path of Path.t
  | Longident of Longident.t

module Path_map = Map.Make(struct type t = path let compare = compare end)

let global_env = ref (lazy (raise Exit))
let local_envs = ref Path_map.empty

(* Returns [acc] plus all modules of [dir] *)
let add_modules_from_directory acc dir =
  let acc = ref acc in
  Array.iter
    (fun fname ->
       if Filename.check_suffix fname ".cmi" then
         acc := String_set.add (String.capitalize (Filename.chop_suffix fname ".cmi")) !acc)
    (Sys.readdir (if dir = "" then Filename.current_dir_name else dir));
  !acc

let valid id =
  id <> "" &&
    (match id.[0] with
       | 'A' .. 'Z' | 'a' .. 'z' |  '_' -> true
       | _ -> false)

let add id set = if valid id then String_set.add id set else set

let add_names_of_type decl acc =
  match decl.type_kind with
    | Type_variant constructors ->
        List.fold_left (fun acc (name, _) -> add name acc) acc constructors
    | Type_record(fields, _) ->
        List.fold_left (fun acc (name, _, _) -> add name acc) acc fields
    | Type_abstract ->
        acc

let rec get_names_of_module_type = function
  | Tmty_signature decls ->
      List.fold_left
        (fun acc decl -> match decl with
           | Tsig_value(id, _)
           | Tsig_exception(id, _)
           | Tsig_module(id, _, _)
           | Tsig_modtype(id, _)
           | Tsig_class(id, _, _)
           | Tsig_cltype(id, _, _) ->
               add (Ident.name id) acc
           | Tsig_type(id, decl, _) ->
               add_names_of_type decl (add (Ident.name id) acc))
        String_set.empty decls
  | Tmty_ident path -> begin
      match try Some (Env.find_modtype path !Toploop.toplevel_env) with Not_found -> None with
        | Some Tmodtype_abstract -> String_set.empty
        | Some Tmodtype_manifest module_type -> get_names_of_module_type module_type
        | None -> String_set.empty
    end
  | _ ->
      String_set.empty

(* List all names of the module with path [path] *)
let get_names_of_module path =
  match
    try
      match path with
        | Path path ->
            Some (Env.find_module path !Toploop.toplevel_env)
        | Longident ident ->
            Some (snd (Env.lookup_module ident !Toploop.toplevel_env))
    with Not_found ->
      None
  with
    | Some module_type -> get_names_of_module_type module_type
    | None -> String_set.empty

let names_of_module path =
  try
    Path_map.find path !local_envs
  with Not_found ->
    let names = get_names_of_module path in
    local_envs := Path_map.add path names !local_envs;
    names

(* List all names accessible without a path *)
let env_names () =
  let rec loop acc = function
    | Env.Env_empty -> acc
    | Env.Env_value(summary, id, _) -> loop (add (Ident.name id) acc) summary
    | Env.Env_type(summary, id, decl) -> loop (add_names_of_type decl (add (Ident.name id) acc)) summary
    | Env.Env_exception(summary, id, _) -> loop (add (Ident.name id) acc) summary
    | Env.Env_module(summary, id, _) -> loop (add (Ident.name id) acc) summary
    | Env.Env_modtype(summary, id, _) -> loop (add (Ident.name id) acc) summary
    | Env.Env_class(summary, id, _) -> loop (add (Ident.name id) acc) summary
    | Env.Env_cltype(summary, id, _) -> loop (add (Ident.name id) acc) summary
    | Env.Env_open(summary, path) -> loop (String_set.union acc (names_of_module (Path path))) summary
  in
  (* Add names of the environment: *)
  let acc = loop String_set.empty (Env.summary !Toploop.toplevel_env) in
  (* Add accessible modules: *)
  List.fold_left add_modules_from_directory acc !Config.load_path

let make_path l =
  match l with
    | [] ->
        invalid_arg "UTop_complete.make_path"
    | ident :: rest ->
        let rec loop path = function
          | [] -> Longident path
          | component :: rest -> loop (Longident.Ldot(path, component)) rest
        in
        loop (Longident.Lident ident) rest

let reset () =
  global_env := lazy(env_names ());
  local_envs := Path_map.empty

(* +-----------------------------------------------------------------+
   | Filtering                                                       |
   +-----------------------------------------------------------------+ *)

(* Filter blanks and comments except for the last token. *)
let rec filter tokens =
  match tokens with
    | [] -> []
    | [((Blanks | Comment true | Doc true), start, stop, src)] -> [(Blanks, start, stop, src)]
    | ((Blanks | Comment true | Doc true), _, _, _) :: rest -> filter rest
    | x :: rest -> x :: filter rest

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

let complete str =
  let tokens = UTop_lexer.lex_string str in
  (* Filter blanks and comments. *)
  let tokens = filter tokens in
  match tokens with

    (* Completion on directive names. *)
    | [(Symbol, _, stop, "#")]
    | [(Symbol, _, _, "#"); (Blanks, _, stop, _)] ->
        (stop, get_directives ())
    | [(Symbol, _, _, "#"); ((Lident | Uident), start, _, src)] ->
        (start, lookup_assoc src (get_directives ()))

    (* Complete with ";;" when possible. *)
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, _); (String true, _, stop, _)]
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, _); (String true, _, _, _); (Blanks, _, stop, _)] ->
        (stop, [(";;", "")])
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, _); (String true, _, _, _); (Symbol, start, _, ";")] ->
        (start, [(";;", "")])

    (* Completion on #require. *)
    | [(Symbol, _, _, "#"); (Lident, _, _, "require"); (String false, start, stop, str)] ->
        let pkg = String.sub str 1 (String.length str - 1) in
        let pkgs = lookup pkg (Fl_package_base.list_packages ()) in
        (start + 1, List.map (fun pkg -> (pkg, "\";;")) (List.sort compare pkgs))

    (* Completion on #load. *)
    | [(Symbol, _, _, "#"); (Lident, _, _, "load"); (String false, start, stop, str)] ->
        let file = String.sub str 1 (String.length str - 1) in
        let list = list_files (fun name -> Filename.check_suffix name ".cma" || Filename.check_suffix name ".cmo") (Filename.dirname file) in
        let name = basename file in
        let result = lookup_assoc name list in
        (stop - Zed_utf8.length name,
         List.map (function (w, Directory) -> (w, "") | (w, File) -> (w, "\";;")) result)

    (* Completion on #use. *)
    | [(Symbol, _, _, "#"); (Lident, _, _, "use"); (String false, start, stop, str)] ->
        let file = String.sub str 1 (String.length str - 1) in
        let list = list_files (fun name -> true) (Filename.dirname file) in
        let name = basename file in
        let result = lookup_assoc name list in
        (stop - Zed_utf8.length name,
         List.map (function (w, Directory) -> (w, "") | (w, File) -> (w, "\";;")) result)

    (* Completion on #directory and #cd. *)
    | [(Symbol, _, _, "#"); (Lident, _, _, ("cd" | "directory")); (String false, start, stop, str)] ->
        let file = String.sub str 1 (String.length str - 1) in
        let list = list_directories (Filename.dirname file) in
        let name = basename file in
        let result = lookup name list in
        (stop - Zed_utf8.length name, List.map (function dir -> (dir, "")) result)

    (* Generic completion on directives. *)
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, dir); (Blanks, _, stop, _)] ->
        (stop,
         match try Some (Hashtbl.find Toploop.directive_table dir) with Not_found -> None with
           | Some (Toploop.Directive_none _) -> [(";;", "")]
           | Some (Toploop.Directive_string _) -> [(" \"", "")]
           | Some (Toploop.Directive_bool _) -> [("true", ";;"); ("false", ";;")]
           | Some (Toploop.Directive_int _) -> []
           | Some (Toploop.Directive_ident _) -> []
           | None -> [])
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, dir); ((Lident | Uident), start, _, id)] ->
        (start,
         match try Some (Hashtbl.find Toploop.directive_table dir) with Not_found -> None with
           | Some (Toploop.Directive_none _) -> []
           | Some (Toploop.Directive_string _) -> []
           | Some (Toploop.Directive_bool _) -> lookup_assoc id [("true", ";;"); ("false", ";;")]
           | Some (Toploop.Directive_int _) -> []
           | Some (Toploop.Directive_ident _) -> []
           | None -> [])

    (* Completion on identifiers. *)
    | [] ->
        (0, List.map (fun w -> (w, "")) (String_set.elements (String_set.union !UTop.keywords (Lazy.force !global_env))))
    | _ ->
        match get_ident [] tokens with
          | None ->
              (0, [])
          | Some ([], id, start) ->
              (start, List.map (fun w -> (w, "")) (lookup id (String_set.elements (String_set.union !UTop.keywords (Lazy.force !global_env)))))
          | Some (path, id, start) ->
              (start, List.map (fun w -> (w, "")) (lookup id (String_set.elements (names_of_module (make_path path)))))
