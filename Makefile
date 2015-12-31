# Copyright © 2015 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

all:
	for d in `seq 1 25`; do ( cd $$d && make ) ; done

clean:
	for d in `seq 1 25`; do ( cd $$d && make clean ) ; done
	cd Soln && make clean
