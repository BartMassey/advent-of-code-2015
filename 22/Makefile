# Copyright © 2015 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HC = ghc
HCFLAGS = -i../Soln -Wall -O2

soln: soln.hs
	$(HC) $(HCFLAGS) --make soln.hs

old-soln: old-soln.hs
	$(HC) $(HCFLAGS) --make old-soln.hs

clean:
	-rm -f soln.hi soln.o soln
	-rm -f old-soln.hi old-soln.o old-soln
	( cd ../Soln && make clean )
