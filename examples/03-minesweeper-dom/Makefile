.PHONY: clean

all: minesweeper.js

%.js: %.ml
	obrowserc -debug 0 $< -o $@

clean:
	@echo "cleaning"
	@rm -f *.cm* *.js *~
