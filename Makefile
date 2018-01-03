all: CD

CD: CD.hs
	ghc --make -main-is CD CD.hs
