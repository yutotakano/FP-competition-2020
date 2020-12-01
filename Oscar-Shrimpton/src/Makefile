
.all: clean run

run:
	ghc -package random -package not-gloss -package linear Main
	./Main

clean:
	rm -f ./*.hi ./*.o ./Main
