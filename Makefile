all:
	echo No 'all' target yet

prepare:
	echo No 'prepare' target yet

run:
	echo No 'run' target yet

test:
	echo No 'test' target yet

clean:
	echo No 'clean' target yet

%.o : %.hs
	ghc $<
