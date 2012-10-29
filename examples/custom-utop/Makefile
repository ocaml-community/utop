OC := ocamlbuild -classic-display -no-links -use-ocamlfind

build:
	$(OC) -pkg threads,utop -cflag -thread -lflag -thread myutop.top

clean:
	$(OC) -clean
