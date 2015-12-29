This problem requires parsing and evaluation of a
*combinatorial* circuit. Because there is no feedback and no
nondeterminism, the evaluation can be memoized for
performance, and no infinite recursion is possible.

I chose to memoize by simply modifying the circuit. One
could also use a memo monad, or just use the state monad
to remember the modified circuit, but it seemed easier
to just return the new circuit with the evaluation. There
is mutual recursion here. There are also some interesting
type issues: having the circuit map just have bare names
on the left is a bit dodgy, but the obvious alternative
involves gratuitous typeclasses.
