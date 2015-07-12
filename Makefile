all:
	ocamlbuild -lib str main.native
clean:
	ocamlbuild -clean
