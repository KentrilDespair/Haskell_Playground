
all: Playground.hs run_tests.hs
	ghc run_tests.hs -o playground

run: all
	./playground

clean:
	rm playground *.hi *.o
