.PHONY:clean

all: obrowser.cma

obrowser.cma: obrowser_jsoo.cmo obrowser_io.cmo obrowser_regexp.cmo stubs.o
	ocamlmklib -o obrowser $^

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

%.o: %.c
	ocamlc -c $< -o $@

stubs.c: $(wildcard *.ml)
	@echo "[STUBS] $^"
	@echo "// stubs extracted from $<" > $@
	@cat $^ | perl -e '%s=();s/\s*external\s+([^(][^:]+|\([^)]+\)):\s*([^=]+)=[^"]*"([^%][^"]+).*/\/\/ Caml name: $$1\n\/\/ Type:   $$2\nvoid $$3 () {\n  exit(1) ;\n}/ && (exists $$s{$$3} || ($$s{$$3}=true && print)) while(<>)' >> $@

clean:
	@echo "[CLEAN]"
	@-rm -f *.cm* *.so *.a *.so *~

install: obrowser.cma
	-mkdir -p $(LIBDIR)/obrowser
	@for i in META *.cmi obrowser.cma dllobrowser.so libobrowser.a ; \
         do \
	   echo install $$i $(LIBDIR)/obrowser/$$i ; \
	   install $$i $(LIBDIR)/obrowser/$$i ; \
	 done

uninstall:
	@for i in META *.cmi obrowser.cma dllobrowser.so libobrowser.a ; \
         do \
	   rm $(LIBDIR)/obrowser/$$i ; \
	   rm $(LIBDIR)/obrowser/$$i ; \
	 done
	-rmdir $(LIBDIR)/obrowser
