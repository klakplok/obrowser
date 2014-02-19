.PHONY: clean

main.js: main.byte
	obrowserc -debug 0 $< -o $@

main.byte: main.ml
	ocamlc -I $(shell obrowserc -where) -vmthread unix.cma threads.cma graphics.cma obrowser.cma  $< -o $@

clean:
	rm -f *.cm* *.js *.byte *~
