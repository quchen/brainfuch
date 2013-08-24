all :
	ghc -O2 Main.hs -o brainfuch
	strip -s brainfuch

clean :
	rm -rf *.o
	rm -rf *.hi
	rm -f brainfuch