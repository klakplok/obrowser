.PHONY: clean

all: main.js

%.js: %.ml
	obrowserc $< -o $@

clean:
	@echo "cleaning"
	@rm -f *.cm* *.js *~
