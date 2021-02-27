
FILES = Playground.hs run_tests.hs Vector.hs
BIN = playground

all: playground

playground: $(FILES)
	ghc run_tests.hs -o $(BIN) 

run: playground
	./$(BIN)

clean:
	rm $(BIN) *.hi *.o
