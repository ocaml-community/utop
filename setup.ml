(*
 * setup.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(* OASIS_START *)

let () =
  let command = Printf.sprintf "oasis setup-dev -run %s %s" Sys.executable_name (String.concat " " (Array.to_list Sys.argv)) in
  Printf.eprintf "I: Running command '%s'\n%!" command;
  exit (Sys.command command)
;;

(* OASIS_STOP *)

let search_compiler_libs () =
  OASISContext.printf `Info "Searching for OCaml compiler libraries";
  let stdlib = BaseEnv.var_get "standard_library" in
  let ( / ) = Filename.concat in
  try
    List.find (fun path -> Sys.file_exists (path / "types.cmi") || Sys.file_exists (path / "typing" / "types.cmi")) [
      stdlib;
      stdlib / "compiler-libs";
      stdlib / "compiler-lib";
      stdlib / ".." / "compiler-libs";
      stdlib / ".." / "compiler-lib";
    ]
  with Not_found ->
    OASISContext.printf `Error "Cannot find compiler libraries! See the README for details.";
    exit 1

let compiler_libs =
  BaseEnv.var_define
    ~short_desc:(fun () -> "compiler libraries")
    "compiler_libs"
    (Lazy.lazy_from_fun search_compiler_libs)

let () = setup ()
