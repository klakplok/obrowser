GEN= \
  obrowser_dllstdlib.js \
  obrowser_dllgraphics.js \
  obrowser_dllvmthreads.js \
  obrowser_dllunix.js \
  obrowser_dllobrowser.js \
  obrowser_dlljs_of_ocaml.js

.PHONY: clean depends

all: $(GEN)

clean:
	@echo "cleaning"
	@-rm -f $(GEN) *~

obrowser_%.js: obrowser_%.in.js
	cpp -DDEBUG -P -I. $< > $@

install: $(GEN)
	-mkdir -p $(LIBDIR)/obrowser
	@for i in $(GEN) ; \
         do \
	   echo install $$i $(LIBDIR)/obrowser/$$i ; \
	   install $$i $(LIBDIR)/obrowser/$$i ; \
	 done

uninstall: $(GEN)
	@for i in $(GEN) ; \
         do \
	   echo rm $(LIBDIR)/obrowser/$$i ; \
	   rm $(LIBDIR)/obrowser/$$i ; \
	 done
	-rmdir $(LIBDIR)/obrowser

depends:
	makedepend -ojs *.in.js

# DO NOT DELETE THIS LINE -- make depend depends on it.

obrowser_dllgraphics.js: graphics.js
obrowser_dllobrowser.js: rtjs.js jsoo.js regexp.js
obrowser_dllstdlib.js: Int64.js md5.js ffi.js internal.js custom.js
obrowser_dllstdlib.js: pervasives.js sys.js obrowser_vio.js
obrowser_dllvmthreads.js: threads.js
