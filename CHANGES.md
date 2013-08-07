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
