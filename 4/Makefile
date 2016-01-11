# Copyright © 2015 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HC = ghc
HCFLAGS = -i../Soln -Wall -O2 -threaded -feager-blackholing -rtsopts

soln: soln.hs
	$(HC) $(HCFLAGS) --make soln.hs

clean:
	-rm -f soln.hi soln.o soln
	( cd ../Soln && make clean )
