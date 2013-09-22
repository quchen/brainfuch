SRC = src
ELF = brainfuch

all :
	ghc -O2 src/Main.hs -o $(ELF) -i$(SRC)
	strip -s $(ELF)

clean :
	rm -f $(ELF)
	rm -f src/*.o
	rm -f src/*.hi