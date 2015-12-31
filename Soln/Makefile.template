HC = ghc
HCFLAGS = -i../Soln -Wall -O2

soln: soln.hs
	$(HC) $(HCFLAGS) --make soln.hs

clean:
	-rm -f soln.hi soln.o soln
	( cd ../Soln && make clean )
