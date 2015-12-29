HC = ghc
HCFLAGS = -i../Soln -Wall -O2

soln: soln.hs
	$(HC) $(HCFLAGS) --make soln.hs

best-recipe: best-recipe.hs
	$(HC) $(HCFLAGS) --make best-recipe.hs

clean:
	-rm -f soln.hi soln.o soln
	-rm -f best-recipe.hi best-recipe.o best-recipe
	( cd ../Soln && make clean )
