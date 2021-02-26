
all: Playground.hs run_tests.hs
	ghc run_tests.hs -o playground

run:
	./playground

clean:
	rm playground *.hi *.o
