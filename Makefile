HC=ghc

all: calc

.PHONY: clean

calc: calculator.hs
	$(HC) $^ -o $@

clean: 
	rm -f *.o *.hi calc
