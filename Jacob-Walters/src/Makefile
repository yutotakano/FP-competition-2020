all:
	stack ghc -- -O2 -optc-ffast-math -fexcess-precision -optc-O3 marboreal.hs

png: all
	./marboreal > out.pgm && convert out.pgm out.png
