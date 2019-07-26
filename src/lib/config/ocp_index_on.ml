open LTerm_read_line
open UTop_token

module String_set = Set.Make(String)
module String_map = Map.Make(String)

let cmd_input_line cmd =
  try
    let ic = Unix.open_process_in (cmd ^ " 2>/dev/null") in
    let r = input_line ic in
    let r =
      let len = String.length r in
      if len>0 && r.[len - 1] = '\r' then String.sub r 0 (len-1) else r
    in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> r
    | _ -> failwith "cmd_input_line"
  with
  | End_of_file | Unix.Unix_error _ | Sys_error _ -> failwith "cmd_input_line"

let index=
  let ocaml_lib= try (cmd_input_line) "ocamlc -where" with _-> "" in
  let opam_lib= try (cmd_input_line) "opam config var lib" with _-> "" in
  LibIndex.load @@ LibIndex.Misc.unique_subdirs [ocaml_lib; opam_lib]

let complete input names_of_module global_names= function
  | [(Symbol "#", _); (Lident "infoof", _); (String (tlen, false), loc)] ->
    let prefix = String.sub input (loc.ofs1 + tlen) (String.length input - loc.ofs1 - tlen) in
    begin match Longident.parse prefix with
    | Longident.Ldot (lident, last_prefix) ->
      let set = names_of_module lident in
      let compls = lookup last_prefix (String_set.elements set) in
      let start = loc.idx1 + 1 + (String.length prefix - String.length last_prefix) in
      Some (start, List.map (fun w -> (w, "")) compls)
    | _ ->
      let set = global_names () in
      let compls = lookup prefix (String_set.elements set) in
      Some (loc.idx1 + 1, List.map (fun w -> (w, "")) compls)
    end
  | _-> None

#if OCAML_VERSION >= (4, 04, 0)
let lookup_type longident env = Env.lookup_type longident env
#else
let lookup_type id env= let path, _= Env.lookup_type in path
#endif

let infoof render_out_phrase print_error sid =
  let id  = Longident.parse sid in
  let env = !Toploop.toplevel_env in
  let from_type_desc = function
    | Types.Tconstr (path, _, _) ->
      let typ_decl = Env.find_type path env in
      path, typ_decl
    | _ -> assert false
  in
  let name=
    try
      let path = lookup_type id env in
      Some (Path.name path)
    with Not_found ->
    try
      let (path, _val_descr) = Env.lookup_value id env in
      Some (Path.name path)
    with Not_found ->
    try
      let lbl_desc = Env.lookup_label id env in
      let (path, _ty_decl) = from_type_desc lbl_desc.Types.lbl_res.Types.desc in
      Some (Path.name path)
    with Not_found ->
    try
      let path = Env.lookup_module id env ~load:true in
      Some (Path.name path)
    with Not_found ->
    try
      let (path, _mty_decl) = Env.lookup_modtype id env in
      Some (Path.name path)
    with Not_found ->
    try
      let cstr_desc = Env.lookup_constructor id env in
      match cstr_desc.Types.cstr_tag with
      | _ ->
        let (path, _ty_decl) = from_type_desc cstr_desc.Types.cstr_res.Types.desc in
        Some (Path.name path)
    with Not_found ->
      None
  in
  let open Lwt in
  match name with
  | None ->
    (try
      let info= LibIndex.Print.info ~color:false (LibIndex.get index sid) in
      Lwt_main.run (Lazy.force LTerm.stdout >>= fun term -> render_out_phrase term info)
    with Not_found->
      Lwt_main.run (Lazy.force LTerm.stdout >>= fun term -> print_error term "Unknown info\n"))
  | Some name ->
    try
      let info= LibIndex.Print.info ~color:false (LibIndex.get index name) in
      Lwt_main.run (Lazy.force LTerm.stdout >>= fun term -> render_out_phrase term info)
    with Not_found->
      Lwt_main.run (Lazy.force LTerm.stdout >>= fun term -> print_error term "Unknown info\n")

let add_directive directive_table render_out_phrase print_error=
  Hashtbl.add directive_table "infoof"
    (Toploop.Directive_string (infoof render_out_phrase print_error))

