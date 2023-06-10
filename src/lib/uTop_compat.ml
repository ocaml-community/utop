let lookup_value =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_value_by_name
#else
  Env.lookup_value
#endif

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

let lookup_module name env =
#if OCAML_VERSION >= (4, 10, 0)
  let path, md = Env.find_module_by_name name env in
#else
  let path = Env.lookup_module name env ~load:true in
  let md = Env.find_module path env in
#endif
  (path, md.md_type)

let lookup_label =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_label_by_name
#else
  Env.lookup_label
#endif

let lookup_modtype =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_modtype_by_name
#else
  Env.lookup_modtype
#endif

let lookup_constructor =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_constructor_by_name
#else
  Env.lookup_constructor
#endif

let lookup_class=
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_class_by_name
#else
  Env.lookup_class
#endif

let longident_parse str =
#if OCAML_VERSION >= (4, 11, 0)
  let lexbuf = Lexing.from_string str in
  Parse.longident lexbuf
#else
  Longident.parse str
#endif

let toploop_all_directive_names () =
#if OCAML_VERSION >= (4, 13, 0)
  Toploop.all_directive_names ()
#else
  Hashtbl.fold (fun dir _ acc -> dir::acc) Toploop.directive_table []
#endif

#if OCAML_VERSION >= (4, 10, 0)
let lookup_type longident env =
  Env.find_type_by_name longident env
#else
let lookup_type longident env =
  let path = Env.lookup_type longident env in
  (path, Env.find_type path env)
#endif

let set_load_path path =
#if OCAML_VERSION >= (5, 0, 0)
  Load_path.init path ~auto_include:Load_path.no_auto_include
#else
  Load_path.init path
#endif

let toploop_use_silently fmt name =
#if OCAML_VERSION >= (4, 14, 0)
  Toploop.use_silently fmt (match name with "" -> Stdin | _ -> File name)
#else
  Toploop.use_silently fmt name
#endif

module Persistent_signature =
#if OCAML_VERSION >= (4, 09, 0)
  Persistent_env.Persistent_signature
#else
  Env.Persistent_signature
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

let iter_structure expr =
#if OCAML_VERSION >= (4,09,0)
  let next iterator e = Tast_iterator.default_iterator.expr iterator e in
  let expr iterator = expr (next iterator) in
  let iter = { Tast_iterator.default_iterator with expr } in
  iter.structure iter
#else
  let module Search =
    TypedtreeIter.MakeIterator(struct
      include TypedtreeIter.DefaultIteratorArgument

      let enter_expression = expr ignore
     end) in
  Search.iter_structure
#endif

(** Returns whether the given path is persistent. *)
let rec is_persistent_path = function
  | Path.Pident id -> Ident.persistent id
  | Path.Pdot (p, _) -> is_persistent_path p
  | Path.Papply (_, p) -> is_persistent_path p
#if OCAML_VERSION >= (5, 1, 0)
  | Path.Pextra_ty (p, _) -> is_persistent_path p
#endif
