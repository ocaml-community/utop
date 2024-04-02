INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: all
all:
	dune build

.PHONY: install
install:
	dune install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall:
	dune uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: examples
examples:
	dune build @examples

.PHONY: test
test:
	dune runtest

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	dune build --workspace dune-workspace.dev

.PHONY: cinaps
cinaps:
	cinaps -styler ocp-indent -i src/migrate_parsetree_versions.ml*
	cinaps -styler ocp-indent -i src/migrate_parsetree_40?_40?.ml*

.PHONY: clean
clean:
	rm -rf _build *.install
	find . -name .merlin -delete

.PHONY: create-switches
create-switches:
	opam switch create utop-412 4.12.0
	opam switch create utop-413 4.13.1
	opam switch create utop-414 4.14.1
	opam switch create utop-500 5.0.0
	opam switch create utop-510 5.1.0
	opam switch create utop-520 5.2.0+trunk

.PHONY: install-switches
install-switches:
	opam install --switch utop-412 --deps-only --with-test -y .
	opam install --switch utop-413 --deps-only --with-test -y .
	opam install --switch utop-414 --deps-only --with-test -y .
	opam install --switch utop-500 --deps-only --with-test -y .
	opam install --switch utop-510 --deps-only --with-test -y .
	opam install --switch utop-520 --deps-only --with-test -y .
