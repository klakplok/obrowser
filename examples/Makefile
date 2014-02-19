
all:
	@for i in *-* ; do \
           echo "building $$i" ; \
           cd "$$i" ; if ! make ; then exit 1 ; fi ; cd .. ; \
         done

clean:
	@for i in *-* ; do \
           echo "cleaning $$i" ; \
           cd "$$i" ; make clean ; cd .. ; \
         done
