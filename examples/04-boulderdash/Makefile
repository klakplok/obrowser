.PHONY: clean

main.js: main.byte
	obrowserc -debug 0 $< -o $@

main.byte: main.ml
	ocamlc -vmthread threads.cma -I $$(obrowserc -where) obrowser.cma $< -o $@

clean:
	rm -f *.cm* *.js *.byte *~
