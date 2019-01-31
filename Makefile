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
