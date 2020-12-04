cabal update
cabal build fp-competition
cabal new-run fp-competition -- par 16000 +RTS -N3

Although for Windows you will need to have freeglut.dll installed, as this does depend on OpenGL. I don't know what added requirements there might be for Linux,  but I doubt there are any.

