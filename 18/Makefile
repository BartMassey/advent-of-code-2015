HC = ghc
HCFLAGS = -i../Soln -Wall -O2

soln: soln.hs
	$(HC) $(HCFLAGS) --make soln.hs

life: life.hs
	$(HC) $(HCFLAGS) --make life.hs

clean:
	-rm -f soln.hi soln.o soln life.hi life.o life
