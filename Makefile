all: CD CDM

%: %.hs
	ghc -threaded --make -main-is $* $(*).hs
