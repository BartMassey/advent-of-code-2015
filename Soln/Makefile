# Copyright © 2015 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HC = ghc
HCFLAGS = -Wall -O2

Soln.hi Soln.o: Soln.hs
	$(HC) $(HCFLAGS) --make Soln.hs

clean: 
	-rm -f Soln.hi Soln.o
