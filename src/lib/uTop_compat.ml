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

let set_load_path path =
#if OCAML_VERSION >= (5, 2, 0) 
  Load_path.init ~auto_include:Load_path.no_auto_include ~visible:path ~hidden:[]
#elif OCAML_VERSION >= (5, 0, 0) 
  Load_path.init path ~auto_include:Load_path.no_auto_include 
#else 
  Load_path.init path
#endif

let get_load_path () = 
#if OCAML_VERSION >= (5, 2, 0) 
  Load_path.get_path_list ()
#else
  Load_path.get_paths ()
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
