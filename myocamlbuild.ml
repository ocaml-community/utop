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

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         | After_rules ->
             (* Copy tags from *.byte to *.top *)
             tag_file
               "src/top/uTop_top.top"
               (List.filter
                  (* Remove the "file:..." tag and syntax extensions. *)
                  (fun tag -> not (String.is_prefix "file:" tag) && not (String.is_suffix tag ".syntax"))
                  (Tags.elements (tags_of_pathname "src/top/uTop_top.byte")));

             (* Use -linkpkg for creating toplevels *)
             flag ["ocaml"; "link"; "toplevel"] & A"-linkpkg";

             let env = BaseEnvLight.load () in
             let path = BaseEnvLight.var_get "compiler_libs" env in
             let stdlib = BaseEnvLight.var_get "standard_library" env in

             let findlib_version = BaseEnvLight.var_get "findlib_version" env in
             let findlib_version =
               Scanf.sscanf findlib_version "%d.%d" (Printf.sprintf "findlib_version=(%d, %d)")
             in

             (* Optcomp *)
             let args =
               S[A"-ppopt"; A"syntax/pa_optcomp.cmo";
                 A"-ppopt"; A"-let"; A"-ppopt"; A findlib_version]
             in
             flag ["ocaml"; "compile"; "pa_optcomp"] args;
             flag ["ocaml"; "ocamldep"; "pa_optcomp"] args;
             flag ["ocaml"; "doc"; "pa_optcomp"] args;
             dep ["ocaml"; "ocamldep"; "pa_optcomp"] ["syntax/pa_optcomp.cmo"];

             (* Add directories for compiler-libraries: *)
             let paths = List.filter Sys.file_exists [path; path / "typing"; path / "parsing"; path / "utils"] in
             let paths = List.map (fun path -> S [A "-I"; A path]) paths in
             flag ["ocaml"; "compile"; "use_compiler_libs"] & S paths;
             flag ["ocaml"; "ocamldep"; "use_compiler_libs"] & S paths;
             flag ["ocaml"; "doc"; "use_compiler_libs"] & S paths;

             let paths = [A "-I"; A "+camlp5"] in
             flag ["ocaml"; "compile"; "use_camlp5"] & S paths;
             flag ["ocaml"; "ocamldep"; "use_camlp5"] & S paths;
             flag ["ocaml"; "doc"; "use_camlp5"] & S paths;

             (* Expunge compiler modules *)
             rule "toplevel expunge"
               ~dep:"src/top/uTop_top.top"
               ~prod:"src/top/uTop_top.byte"
               (fun _ _ ->
                  (* Build the list of explicit dependencies. *)
                  let packages =
                    Tags.fold
                      (fun tag packages ->
                         if String.is_prefix "pkg_" tag && not (String.is_suffix tag ".syntax") then
                           String.after tag 4 :: packages
                         else
                           packages)
                      (tags_of_pathname "src/top/uTop_top.byte")
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
                  (* These are not in the stdlib path since 4.00 *)
                  let modules = StringSet.add "Toploop" modules in
                  let modules = StringSet.add "Topmain" modules in
                  Cmd (S [A (stdlib / "expunge");
                          A "src/top/uTop_top.top";
                          A "src/top/uTop_top.byte";
                          A "UTop"; A "UTop_private"; S(List.map (fun x -> A x) (StringSet.elements modules))]));

             rule "full toplevel (not expunged)"
               ~dep:"src/top/uTop_top.top"
               ~prod:"src/top/uTop_top_full.byte"
               (fun _ _ -> cp "src/top/uTop_top.top" "src/top/uTop_top_full.byte")
         | _ ->
             ())
