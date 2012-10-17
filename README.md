utop - a universal toplevel for OCaml
=====================================

utop is an improved toplevel for OCaml. It can run in a terminal or
in Emacs. It supports line edition, history, real-time and context
sensitive completion, colors, and more.

It integrates with the tuareg and typerex modes in Emacs.

Dependencies
------------

* [OCaml](http://caml.inria.fr/ocaml/) (>= 3.12)
* [findlib](http://projects.camlcity.org/projects/findlib.html)
* [react](http://erratique.ch/software/react)
* [lwt](http://ocsigen.org/lwt/) (>= 2.4.0) built with react support
* [Camomile](http://github.com/yoriyuki/Camomile) (>= 0.8)
* [zed](http://github.com/diml/zed) (>= 1.2)
* [lambda-term](http://github.com/diml/lambda-term) (>= 1.2)

For building the development version, you also need to install
[oasis](http://oasis.forge.ocamlcore.org/) (>= 0.3.0).

utop also requires OCaml compiler libraries. Since OCaml 4.00 they are
already installed, for previous versions:

* if you are using debian, they are available as the package
  ocaml-compiler-libs,
* if you are using godi, they are installed by default,
* if you installed ocaml by hand, you can run the script
  `utils/install-compiler-libs.sh`.

Installation
------------

To build and install Lambda-Term:

    $ ./configure
    $ make
    $ make install

### Documentation and manual pages _(optional)_

To build the documentation:

    $ make doc

It will then be installed by `make install`.

### Tests _(optionnal)_

To build and execute tests:

    $ ./configure --enable-tests
    $ make test

Usage
-----

To use utop, simply run:

    $ utop

utop display a bar after the prompt which is used to show possible
completions in real-time. You can navigate in it using `Alt+Left` and
`Alt+Right`, and select one completion using `Alt+Tab`.

Customization
-------------

To add colors to utop, copy one of the files `utoprc-dark` or
`utoprc-light` to `~/.utoprc`. `utoprc-dark` is for terminals with
dark colors (such as white on black) and `utoprc-light` is for
terminals with light colors (such as black on white).

You can also customize the prompt of utop by setting the reference
`UTop.prompt`.

Integration with emacs
----------------------

To use utop in emacs, add the following line to your ~/.emacs file:

```scheme
(autoload 'utop "utop" "Toplevel for OCaml" t)
```

Then you can run utop by executing the command `utop` in emacs.

Integration with the tuareg/typerex mode
----------------------------------------

You can replace the default toplevel used by the tuareg or typerex
mode by utop, for that add the following lines to your `~/.emacs` file:

```scheme
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)
```

You can also complete text in a tuareg or typerex buffer using the
environment of the toplevel. For that bind the function
`utop-edit-complete` to the key you want.
