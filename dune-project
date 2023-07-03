(lang dune 2.0)
(formatting (enabled_for dune))
(name utop)

(generate_opam_files)
(source (github ocaml-community/utop))
(documentation https://ocaml-community.github.io/utop/)
(license BSD-3-Clause)
(maintainers jeremie@dimino.org)
(authors "Jérémie Dimino")

(package
 (name utop)
 (synopsis "Universal toplevel for OCaml")
 (description
  "utop is an improved toplevel (i.e., Read-Eval-Print Loop or REPL) for OCaml. It can run in a terminal or in Emacs. It supports line edition, history, real-time and context sensitive completion, colors, and more. It integrates with the Tuareg mode in Emacs.")
 (depends
  (ocaml (>= 4.11.0))
  base-unix
  base-threads
  (ocamlfind (>= 1.7.2))
  (lambda-term (and (>= 3.1.0) (< 4.0)))
  logs
  lwt
  lwt_react
  (zed (>= 3.2.0))
  (react (>= 1.0.0))
  (cppo (>= 1.1.2))
  (alcotest :with-test)))
