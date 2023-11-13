let get_desc x =
#if OCAML_VERSION >= (4, 14, 0)
  Types.get_desc x
#else
  x.Types.desc
#endif

let toploop_get_directive name =
#if OCAML_VERSION >= (4, 13, 0)
  Toploop.get_directive name
#else
  try Some (Hashtbl.find Toploop.directive_table name) with Not_found -> None
#endif

let toploop_all_directive_names () =
#if OCAML_VERSION >= (4, 13, 0)
  Toploop.all_directive_names ()
#else
  Hashtbl.fold (fun dir _ acc -> dir::acc) Toploop.directive_table []
#endif

let get_load_path () =
#if OCAML_VERSION >= (5, 2, 0)
  let {Load_path.visible; hidden} = Load_path.get_paths () in
  visible @ hidden
#else
  Load_path.get_paths ()
#endif

let set_load_path visible =
#if OCAML_VERSION >= (5, 2, 0)
  Load_path.init ~auto_include:Load_path.no_auto_include ~visible ~hidden:[]
#elif OCAML_VERSION >= (5, 0, 0)
  Load_path.init ~auto_include:Load_path.no_auto_include visible
#else
  Load_path.init visible
#endif

let toploop_use_silently fmt name =
#if OCAML_VERSION >= (4, 14, 0)
  Toploop.use_silently fmt (match name with "" -> Stdin | _ -> File name)
#else
  Toploop.use_silently fmt name
#endif

let toploop_set_paths () =
#if OCAML_VERSION >= (5, 0, 0)
  Toploop.set_paths ~auto_include:Load_path.no_auto_include ()
#else
  Toploop.set_paths ()
#endif

let toploop_load_file ppf fn =
#if OCAML_VERSION >= (4, 13, 0)
  Toploop.load_file ppf fn
#else
  Topdirs.load_file ppf fn
#endif

(** Returns whether the given path is persistent. *)
let rec is_persistent_path = function
  | Path.Pident id -> Ident.persistent id
  | Path.Pdot (p, _) -> is_persistent_path p
  | Path.Papply (_, p) -> is_persistent_path p
#if OCAML_VERSION >= (5, 1, 0)
  | Path.Pextra_ty (p, _) -> is_persistent_path p
#endif

let invalid_package_error_to_string err =
#if OCAML_VERSION >= (5, 2, 0)
  (* NOTE: from https://github.com/ocaml/ocaml/blob/9b059b1e7a66e9d2f04d892a4de34c418cd96f69/parsing/parse.ml#L149 *)
  let invalid ppf ipt = match ipt with
    | Syntaxerr.Parameterized_types ->
      Format.fprintf ppf "parametrized types are not supported"
    | Constrained_types ->
      Format.fprintf ppf "constrained types are not supported"
    | Private_types ->
      Format.fprintf ppf  "private types are not supported"
    | Not_with_type ->
      Format.fprintf ppf "only %a constraints are supported"
        Misc.Style.inline_code "with type t ="
    | Neither_identifier_nor_with_type ->
        Format.fprintf ppf
          "only module type identifier and %a constraints are supported"
          Misc.Style.inline_code "with type"
  in
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "Invalid package type: %a%!" invalid err;
  Buffer.contents buf
#else
  err
#endif

module Exp = struct
  open Ast_helper
#if OCAML_VERSION >= (5, 2, 0)
  open Parsetree
  let fun_ ~loc p e =
   let args = [{
     pparam_loc=loc;
     pparam_desc=Pparam_val (Nolabel, None, p);
   }] in
   (Exp.function_ args None (Pfunction_body e))
#else
  let fun_ ~loc p e = Exp.fun_ ~loc Nolabel None p e
#endif
end

let abstract_type_kind =
#if OCAML_VERSION >= (5, 2, 0)
  Types.(Type_abstract Definition)
#else
  Types.Type_abstract
#endif

let find_in_path_normalized =
#if OCAML_VERSION >= (5, 2, 0)
      Misc.find_in_path_normalized
#else
      Misc.find_in_path_uncap
#endif   

let visible_paths_for_cmt_infos (cmt_infos: Cmt_format.cmt_infos) =
#if OCAML_VERSION >= (5, 2, 0)
  cmt_infos.cmt_loadpath.visible
#else
  cmt_infos.cmt_loadpath
#endif

let add_cmi_hook f =
  let default_load = !Persistent_env.Persistent_signature.load in
#if OCAML_VERSION >= (5, 2, 0)
  let load ~allow_hidden ~unit_name =
    let res = default_load ~unit_name ~allow_hidden in
#else
  let load ~unit_name =
    let res = default_load ~unit_name in
#endif
    (match res with None -> () | Some x -> f x.cmi);
    res
  in
  Persistent_env.Persistent_signature.load := load

