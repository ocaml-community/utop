INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: all
all:
	jbuilder build @install

.PHONY: install
install:
	jbuilder install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install


.PHONY: examples
examples:
	jbuilder build @examples

.PHONY: test
test:
	jbuilder runtest

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	jbuilder build --workspace jbuild-workspace.dev

.PHONY: cinaps
cinaps:
	cinaps -styler ocp-indent -i src/migrate_parsetree_versions.ml*
	cinaps -styler ocp-indent -i src/migrate_parsetree_40?_40?.ml*

.PHONY: clean
clean:
	rm -rf _build *.install
	find . -name .merlin -delete

# This needs to be updated
.PHONY: gh-pages
gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp -t .gh-pages/ _build/utop-api.docdir/*
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages
