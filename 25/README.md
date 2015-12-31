The tricky part of this problem is the index calculation. I
am embarassed to say how long it took me to get this simple
Gaussian sum correct. The strategy is to compute the
diagonal to which the given row and column belong, and then
walk up the diagonal to the correct position.

My original Haskell solution was very short, had the input
values hard-coded in, and ran in about 3 seconds. Someone on
the Interwebs pointed out that modular exponentiation would
be way faster, so I coded that for this solution. I also dug
the row and column out of the input in a very kludgy
way. The result runs in about a millisecond.
