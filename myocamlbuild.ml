(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(* OASIS_START *)
(* OASIS_STOP *)

(* List of toplevels. *)
let toplevels = ["console"; "emacs"; "gtk"]

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         | After_rules ->
             (* Copy tags from *.byte to *.top *)
             List.iter
               (fun name ->
                  let src = "src" / name / ("uTop_" ^ name ^ "_top.byte")
                  and dst = "src" / name / ("uTop_" ^ name ^ "_top.top") in
                  tag_file
                    dst
                    (List.filter
                       (* Remove the "file:..." tag *)
                       (fun tag -> not (String.is_prefix "file:" tag))
                       (Tags.elements (tags_of_pathname src))))
               toplevels;

             (* Use -linkpkg for creating toplevels *)
             flag ["ocaml"; "link"; "toplevel"] & A"-linkpkg";

             (* Optcomp *)
             flag ["ocaml"; "compile"; "pa_optcomp"] & S[A"-ppopt"; A "syntax/pa_optcomp.cmo"];
             flag ["ocaml"; "ocamldep"; "pa_optcomp"] & S[A"-ppopt"; A "syntax/pa_optcomp.cmo"];
             flag ["ocaml"; "doc"; "pa_optcomp"] & S[A"-ppopt"; A "syntax/pa_optcomp.cmo"];
             dep ["ocaml"; "ocamldep"; "pa_optcomp"] ["syntax/pa_optcomp.cmo"];

             let env = BaseEnvLight.load () in
             let path = BaseEnvLight.var_get "compiler_libs" env in
             let stdlib = BaseEnvLight.var_get "standard_library" env in

             (* Add directories for compiler-libraries: *)
             let paths = List.filter Sys.file_exists [path; path / "typing"; path / "parsing"; path / "utils"] in
             let paths = List.map (fun path -> S [A "-I"; A path]) paths in
             flag ["ocaml"; "compile"; "use_compiler_libs"] & S paths;
             flag ["ocaml"; "ocamldep"; "use_compiler_libs"] & S paths;
             flag ["ocaml"; "doc"; "use_compiler_libs"] & S paths;

             (* Expunge compiler modules *)
             rule "toplevel expunge"
               ~dep:"%.top"
               ~prod:"%.byte"
               (fun env _ ->
                  (* Build the list of explicit dependencies. *)
                  let packages =
                    Tags.fold
                      (fun tag packages ->
                         if String.is_prefix "pkg_" tag then
                           String.after tag 4 :: packages
                         else
                           packages)
                      (tags_of_pathname (env "%.byte"))
                      []
                  in
                  (* Build the list of dependencies. *)
                  let deps = Findlib.topological_closure (List.rev_map Findlib.query packages) in
                  (* Build the set of locations of dependencies. *)
                  let locs = List.fold_left (fun set pkg -> StringSet.add pkg.Findlib.location set) StringSet.empty deps in
                  (* Directories to search for .cmi: *)
                  let directories = StringSet.add stdlib (StringSet.add (stdlib / "threads") locs) in
                  (* Construct the set of modules to keep by listing
                     .cmi files: *)
                  let modules =
                    StringSet.fold
                      (fun directory set ->
                         List.fold_left
                           (fun set fname ->
                              if Pathname.check_extension fname "cmi" then
                                StringSet.add (module_name_of_pathname fname) set
                              else
                                set)
                           set
                           (Array.to_list (Pathname.readdir directory)))
                      directories StringSet.empty
                  in
                  Cmd (S [A (stdlib / "expunge");
                          A (env "%.top");
                          A (env "%.byte");
                          A "UTop"; S(List.map (fun x -> A x) (StringSet.elements modules))]))
         | _ ->
             ())
