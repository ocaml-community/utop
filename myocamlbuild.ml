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
       Ocamlbuild_cppo.dispatcher hook;
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
             let stdlib = BaseEnvLight.var_get "standard_library" env in

             let ocaml_version =
               Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun major minor patchlevel ->
                  (* e.g. #define OCAML_VERSION 040201 *)
                 Printf.sprintf "OCAML_VERSION %d" (major * 10000 + minor * 100 + patchlevel))
             in

             (* Cppo *)
             flag ["cppo"] & S[A"-D"; A ocaml_version];

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
                         if String.is_prefix "package(" tag then
                           String.sub tag 8 (String.length tag - 9) :: packages
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
