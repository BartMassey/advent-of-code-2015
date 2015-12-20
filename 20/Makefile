HC = ghc
HCFLAGS = -i.. -Wall -O2

soln: soln.hs Factor.hs
	$(HC) $(HCFLAGS) --make soln.hs

clean:
	-rm -f soln.hi soln.o soln Factor.o Factor.hi
