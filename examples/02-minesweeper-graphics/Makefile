.PHONY: clean

main.js: main.byte
	obrowserc -debug 0 $< -o $@

main.byte: main.ml
	ocamlc graphics.cma $< -o $@

clean:
	rm -f *.cm* *.js *.byte *~
