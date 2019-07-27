open LTerm_read_line
open UTop_token

module String_set = Set.Make(String)
module String_map = Map.Make(String)

let cmd_input_line cmd =
  try
    let ic = Unix.open_process_in cmd in
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

let complete input names_of_module global_names= function
  | [(Symbol "#", _); (Lident "info", _); (String (tlen, false), loc)] ->
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
let lookup_type id env= let path, _= Env.lookup_type id env in path
#endif

let req_query= ref stdout
let rep_query= ref stdin

let query_info render_out_phrase print_error sid =
  let sid= String.trim sid in
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
  let name= match name with Some name-> name | None-> sid in
  let open Lwt in
  output_string !req_query @@ name ^ "\n"; flush !req_query;
  match input_value !rep_query with
  | Some info->
    Lwt_main.run (Lazy.force LTerm.stdout >>= fun term -> render_out_phrase term info)
  | None->
    Lwt_main.run (Lazy.force LTerm.stdout >>= fun term -> print_error term "Unknown info\n")

let add_directive directive_table render_out_phrase print_error=
  Hashtbl.add directive_table "info"
    (Toploop.Directive_string (query_info render_out_phrase print_error))

let child req_query rep_query=
  let req_query= Unix.in_channel_of_descr req_query
  and rep_query= Unix.out_channel_of_descr rep_query in
  let index=
    let ocaml_lib= try (cmd_input_line) "ocamlc -where" with _-> "" in
    let opam_lib= try (cmd_input_line) "opam config var lib" with _-> "" in
    LibIndex.load @@ LibIndex.Misc.unique_subdirs [ocaml_lib; opam_lib]
  in
  let query_info name=
    (try
      let info= LibIndex.Print.info ~color:false (LibIndex.get index name) in
      output_value rep_query (Some info)
    with Not_found->
      output_value rep_query None);
    flush rep_query;
  in
  let rec watching ()=
    let query= input_line req_query in
    query_info query;
    watching ()
  in
  watching ()

let init_ocp_index ()=
  let r1, w1= Unix.pipe ()
  and r2, w2= Unix.pipe () in
  match Unix.fork () with
  | 0->
    let req_query= r1 and rep_query= w2
    in child req_query rep_query
  | child->
    req_query:= Unix.out_channel_of_descr w1;
    rep_query:= Unix.in_channel_of_descr r2;
    child

