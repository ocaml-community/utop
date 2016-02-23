OC := ocamlbuild -classic-display -no-links -use-ocamlfind

build:
	$(OC) -pkg threads,compiler-libs.toplevel,utop test_program.byte

clean:
	$(OC) -clean
