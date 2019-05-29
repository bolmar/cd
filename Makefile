all: CD CDM

%: %.hs
	ghc --make -main-is $* $(*).hs
