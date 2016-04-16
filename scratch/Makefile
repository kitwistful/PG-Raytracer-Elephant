SOURCES = mockoutput.hs quadtreedemo.hs teapot.hs
OBJECTS = $(subst .hs,.o,$(SOURCES))
EXECS = $(subst .hs,,$(SOURCES))

all: $(OBJECTS)
	
%.o : %.hs
	for i in $(SOURCES); do \
		ghc $$i; \
	done
	
clean:
	rm $(EXECS)
	rm *.o *.hi