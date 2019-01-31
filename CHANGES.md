2.3.0 (2019-01-31)
------------------

The new feature in this release is to automatically install
printers marked with `[@@ocaml.toplevel_printer]` (#269 @diml).
Adding this annotation to your libraries will remove the need
to have a separate `top` package to install the printers.

For example, in the [uri](https://github.com/mirage/ocaml-uri)
library, the old printing function for `Uri.t` was:

```
val pp_hum : Format.formatter -> t -> unit
```

Just adding this annotation results in `Uri.t` values being automatically
pretty printed in this version of utop.

```
val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
```

* Add cool screenshot to README (#259 @rizo) and update links (#257 @bobot)
* Improve robustness by using more tail-recursive functions (#251 @gpetiot)
* Remove deprecation warnings in newer compilers (#246 @ncihnegn)
* Minimum OCaml version supported is now 4.03.0 (#254 @XVilka)
* Publish API documentation online and add `doc:` entry to opam file (#270 @avsm)
* Port build to dune from jbuilder (#268 @avsm)
* Upgrade local opam metadata to opam 2.0 format (#268 @avsm)

2.2.0 (2018-07-15)
------------------

* 4.07.0 compatibility (#238, @hcarty)
* Fix compatibility with latest tuareg-mode (#241, @Wilfred)
* Do not expand include directories (#242, @sliquister)

2.1.0 (2018-02-28)
------------------

* Add support for company-mode based completion in utop.el (#233)

2.0.2 (2017-11-07)
------------------

* 4.06.0 compatibility (#221)

2.0.1 (2016-05-30)
------------------

* Fix: restore the installation of `utop.el` (#210, Louis Gesbert)

2.0.0 (2016-05-26)
------------------

* Add `-implicit-bindings` option to automatically bind expressions to names
  `_0`, `_1` and so on. For example, `3 + 4;;` becomes `let _0 = 3 + 4;;`
  (#161, #193, Fabian Hemmer)
* Add tab completion for `#mod_use` (#181, Leonid Rozenberg)
* Mention `#help` in `#utop_help` (#190, Fabian Hemmer)
* Add `#utop_stash` and `#utop_save` to save the session to a file
  (#169, #199, Christopher Mcalpine and Fabian Hemmer)
* Add support for reason in the emacs mode (#206, Andrea Richiardi)
* Fix a bug where utop wouldn't apply ppx rewriters when running in
  emacs (Bug report: #192, fix: #202, Deokhwan Kim)
* Refactor the use of hooks to support the various OCaml emacs mode
  (#201, Andrea Richiardi)
* Drop support for camlp4/camlp5
* Drop support for OCaml <= 4.01
* Switch the build system to jbuilder
* Resurect `UTop_main.interact`

1.19.3 (2016-08-15)
-------------------

* fix compatibility with 4.04.0+beta1

1.19.2 (2016-04-25)
-------------------

* Make ppx\_tools dependency optional

1.19.1 (2016-04-18)
-------------------

* fix compatibility with 4.03.0+beta2

1.19 (2016-04-07)
-----------------

* allow to configure the external editor with `UTop.set_external_editor`
* add `UTop.set_margin_function` to allow users to set
  the margin for the toplevel outcome. It is 80 by default
* better for quoted strings (`{|...|}`)
* add a `#pwd` directive
* experimental support for running utop in the middle of a program
  with `UTop_main.interact`
* fix Async integration (automatic waiting of `_ Deferred.t` value).
  The new version is more robust against future change in Async
* fix use of the non-existing `replace-in-string` function in the
  emacs mode (Syohei Yoshida)
* fallback to Latin-1 for invalid UTF-8 sequences in the compiler output

1.18.2 (2016-03-02)
-------------------

* fix compatibility with OCaml 4.03

1.18.1 (2015-11-03)
-------------------

* fix compatibility with findlib 1.5.6

1.18 (2015-06-23)
-----------------

* emace mode improvements (Mads Hartmann Jensen)
  - add `utop-minor-mode` to make integration with major modes cleaner
  - clean-up of the elisp code
* add `UTop.end_and_accept_current_phrase` to avoid typing `;;` at the
  end of every phrases
* fix compatibility with OCaml trunk

1.17 (2014-12-12)
-----------------

* re-export `Config.load_path` as `UTop.load_path` (Peter Zotov)
* enable utop-command to be buffer-local (Mads Hartmann Jensen)
* fix 4.01 compatibility (Peter Zotov)

1.16 (2014-10-20)
-----------------

* make camlp4 support optional
* require OCaml 4.01.0 or newer
* implement wrapper for -safe-string

1.15 (2014-08-30)
-----------------

* fix compatibility with OCaml 4.02.0

1.14 (2014-07-05)
-----------------

* fix compatibility with OCaml 4.00.1 and earlier

1.13 (2014-07-04)
-----------------

* don't try to colorize the output when there is too much
* add auto-completion for the `#ppx` directive
* add support for -ppx, -dparsetree and -dsource
* fix compatibility with OCaml 4.02
* update pa_optcomp
* do not display the camlp4 welcome message

1.12 (2014-04-21)
-----------------

* supports -require for scripts
* support for React 1.0.0
* make utop.el compatible with melpa: http://melpa.milkbox.net

1.11 (2014-02-11)
-----------------

* update the async hook following the renaming of `Async_core` to
  `Async_kernel`
* fix tab completion not working on some emacs
* complete `#load_rec` the same way as `#load`

1.10 (2013-12-10)
-----------------

* add the `-require` command line argument to specify packages on the
  command line

1.9 (2013-11-26)
----------------

* automatically load all files in `$OCAML_TOPLEVEL_PATH/autoload` at
  startup. Can be disabled with `autoload: false` in `~/.utoprc` or
  `-no-autoload`.
* fix #38: handle errors from custom camlp4 ast filters
* fix #7: avoid a stack overflow in UTop_lexer

1.8 (2013-10-25)
----------------

* handle new syntax errors
* extend `#typeof` to values and modules. Thanks to Thomas Refis for
  this feature

1.7 (2013-08-08)
----------------

* fix compilation with ocaml < 4.01

1.6 (2013-08-07)
----------------

* hide topfind messages by default
* more predefined prompts available via `#utop_prompt_XXX`
* fix a bug in `#require` when passing multiple packages
* display errors in ~/.lambda-term-inputrc nicely
* doc update
* fix an issue when using first-class modules

1.5 (2013-04-28)
----------------

* when evaluating a region/buffer in emacs, evaluate all phrases
  instead of just the first one. Thanks to Matthias Andreas Benkard
  for this feature
* change the default prompt from `#` to `$` to match the standard
  toplevel
* add the option `UTop.show_box` to allow one to hide the completion
  bar
* enhance the lwt/async hooks for automatically waiting on a
  thread/deferred. Hooks were not triggered when the type of the
  expression was a type alias

1.4 (2013-03-09)
----------------

* hide identifiers starting with `_`. This can be disabled with
  `UTop.set_hide_reserved false`.
* automatically load camlp4 parsing (with original syntax) when
  trying to load a syntax extension
* fix a small bug when using camlp4, causing an exception to be raised
  when pressing `Enter` in the middle of a comment

1.3 (2013-01-29)
----------------

* allow to automatically wait for ascync deferred values
* added the `-short-paths` options for OCaml >= 4.01.0
  (and make it the default)

1.2.1 (2012-07-31)
------------------

* fix: do not expunge `Toploop`
* install a non-expunged version of utop: `utop-full`

1.2 (2012-07-30)
----------------

* ocaml 4.00 compatibility
* prevent findlib from being initialized twice
* better highlighting of errors
* automatically insert `Lwt_main.run` for
  toplevel expressions of type `'a Lwt.t`
* better camlp4 support
    * parse quotations and antiquotations to
      handle completion inside them
    * better support revised syntax
* emacs mode improvements
    * various fixes
    * highlight errors
    * add a menu
    * add interactive list of findlib packages
    * packages can be pre-loaded via the file variable
      `utop-package-list`
    * better tuareg integration
    * typerex integration
    * allow to complete using the toplevel environment
      in a tuareg buffer
    * allow to change the utop command
    * use the same history as the terminal mode
    * follow output of ocaml

1.1 (2010-08-06)
----------------

* add completion on labels
* add completion on methods
* smarter completion on record fields
* fix a bug in the lexer
* improvement for the emacs mode:
    * now pressing Tab really complete input
    * when sending input from a tuareg buffer, the cursor follow the
      end of buffer in all utop windows
    * fix usage of threads
    * add help
    * add manual pages
    * show more information in the prompt:
        * show the current value of the macro counter
        * show the nnumber of key pressed since the beginning of a macro
          when recording a macro
        * show intermediate key sequence
    * better support for light colors terminals
    * add colors for module name and directives
    * add `UTop.smart_accept` to send only lines terminating with a `;;` token
    * search for compiler libraries at configure time
    * add a script to install compiler libraries
    * fix compatibility with ocaml 3.13
