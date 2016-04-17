all:
	echo No 'all' target yet

prepare:
	echo No 'prepare' target yet

run:
	cd ./src ; \
	pwd ; \
	runhaskell QuadTreeDemo.hs

test:
	echo No 'test' target yet

clean:
	echo No 'clean' target yet

%.o : %.hs
	ghc $<
