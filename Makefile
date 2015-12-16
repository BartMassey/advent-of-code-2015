HC = ghc
HCFLAGS = -i.. -Wall -O

soln: soln.hs
	$(HC) $(HCFLAGS) --make soln.hs

clean:
	-rm -f soln.hi soln.o soln
