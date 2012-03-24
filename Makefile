EXE := ddx

.PHONY: all clean
all:
	ghc --make -o ddx main

clean:
	rm -f *.hi *.o $(EXE)
