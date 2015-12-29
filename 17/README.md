This problem is SUBSET-SUM, which is NP-complete in
general. So as usual we just brute-force it.

Haskell can essentially be used as a powerful pocket
calculator here. Because of GHC's efficient compilation of
these kinds of codes, we get near-instantaneous answers
evaluating approximately one million possibilities.
