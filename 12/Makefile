# Copyright © 2015 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HC = ghc
HCFLAGS = -i../Soln -Wall -O2

all: soln soln-aeson soln-json

soln: soln.hs
	$(HC) $(HCFLAGS) --make soln.hs

soln-aeson: soln-aeson.hs
	$(HC) $(HCFLAGS) --make soln-aeson.hs

soln-json: soln-json.hs
	$(HC) $(HCFLAGS) --make soln-json.hs

clean:
	-rm -f *.hi *.o soln soln-aeson soln-json
