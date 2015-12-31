N-way Number Partitioning is a classic NPC problem. Rich
Korf has a couple of great papers on efficient solution for
reasonable-sized hard instances. Unfortunately, we need to
examine all solutions, so efficiency is in general not too
likely.

That said, my Haskell solution has unacceptably long
runtimes: about 1 minute for Part A and 7 minutes for Part
B. It does general n-way partitioning using an absolutely
terrible packing heuristic, with a little symmetry-breaking
to speed things up a bit. It should be doing best-fit, but
is actually doing worst-fit. Yuck.

The list monad is used to good effect in cleaning up the
code. That said, I'm not taking advantage of the fact that
all the package weights in the input are different: I could
use sets and a set monad from Hackage to make things more
efficient.

I strongly suspect that with another couple hours of coding
I could get the runtime to under a second, but honestly it's
more work than I'm willing to do right now.
