(*
 * setup.ml
 * --------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(* OASIS_START *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
(* OASIS_STOP *)

let search_compiler_libs () =
  prerr_endline "I: Searching for OCaml compiler libraries";
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
    prerr_endline "E: Cannot find compiler libraries! See the README for details.";
    exit 1

let compiler_libs =
  BaseEnv.var_define
    ~short_desc:(fun () -> "compiler libraries")
    "compiler_libs"
    search_compiler_libs

let () = setup ()
