all:
	for d in `seq 1 25`; do ( cd $$d && make ) ; done

clean:
	for d in `seq 1 25`; do ( cd $$d && make clean ) ; done
	cd Soln && make clean
