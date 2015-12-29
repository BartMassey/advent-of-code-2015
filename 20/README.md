Hey, look! A [Project Euler](http://projecteuler.net/)
problem snuck in here.

The brute-force solution is ugly, but instead notice that a
house will get presents equal to 10 (resp. 11) the sum of
all the factors (including composite factors) of its house
number.

That said, this Haskell solution is still really slow: about
a minute of runtime. There's probably something much faster
given enough careful number-theoretic thinking, but I'll
just be dumb.

The module `Factor.hs` was borrowed from my Project
Euler solutions and upgraded a bit for clarity and
correctness. Turns out I'm a better Haskell programmer
in 2015 than in 2008: who knew?
