OC := ocamlbuild -classic-display -no-links -use-ocamlfind

build:
	$(OC) -tag thread -pkg threads,utop myutop.top

clean:
	$(OC) -clean
