EXEC_SOURCES = mockoutput.hs quadtreedemo.hs teapot.hs
SOURCES =  QuadTree.hs $(EXEC_SOURCES)
OBJECTS = $(subst .hs,.o,$(SOURCES))
EXEC_OBJECTS = $(subst .hs,,$(EXEC_SOURCES))

all: $(OBJECTS)

%.o : %.hs
	ghc $<

clean:
	rm $(EXEC_OBJECTS)
	rm *.o *.hi
