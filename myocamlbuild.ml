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
             (* Use -linkpkg for creating toplevels *)
             flag ["ocaml"; "link"; "toplevel"] & A"-linkpkg";

             let env = BaseEnvLight.load () in
             let path = BaseEnvLight.var_get "compiler_libs" env in
             let stdlib = BaseEnvLight.var_get "standard_library" env in

             (* Add directories for compiler-libraries: *)
             let paths = [A "-I"; A path;
                          A "-I"; A (path / "typing");
                          A "-I"; A (path / "parsing");
                          A "-I"; A (path / "utils")] in
             List.iter
               (fun stage -> flag ["ocaml"; stage; "use_compiler_libs"] & S paths)
               ["compile"; "ocamldep"; "doc"; "link"];

             (* Expunge compiler modules *)
             rule "toplevel expunge"
               ~dep:"%.top"
               ~prod:"%.byte"
               (fun env _ ->
                  (* Build the list of dependencies. *)
                  let deps = Findlib.topological_closure [Findlib.query "lambda-term";
                                                          Findlib.query "findlib";
                                                          Findlib.query "threads"] in
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
                  Cmd(S[A(stdlib / "expunge");
                        A(env "%.top");
                        A(env "%.byte");
                        A"UTop"; A"Outcometree"; A"Topdirs"; A"Toploop";
                        S(List.map (fun x -> A x) (StringSet.elements modules))]))
         | _ ->
             ())

