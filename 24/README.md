N-way Number Partitioning is a classic NP-complete
problem. Rich Korf has a couple of great papers on efficient
solution for reasonable-sized hard instances. Unfortunately,
we need to examine all solutions, so efficiency in general
is not too likely.

The first Haskell solution I tried involved examining all
possible packings, with some really incomplete symmetry
breaking to speed things up a bit. It had bad runtimes:
about 1 minute for Part A and 7 minutes for Part B.

The solution here uses a more sophisticated strategy that
takes advantage of the entanglement scoring. The key ideas
are:

* A small number of larger values with a given sum will
  usually have a smaller product than a large number of
  smaller values. (This is especially true for this problem,
  where all the packages have prime weight, but it is true
  in general as well.)  Thus we can get a low-entanglement
  single bin by trying to jam all the largest values in the
  bin first.

* If we examine all possible packings of a single bin first,
  we won't miss any solutions by extending the packing to
  the remaining bins. Thus, we can assume that we are
  packing a smallest-entanglement bin first. When the
  entanglement of that bin gets larger than the best bin we
  have examined so far, we can abandon the whole packing.

The overall strategy becomes:

1. Find a starting packing with some score *e*. This is
   really fast even with dumb brute-force methods.

2. Using the observations above, try to pack a bin with an
   entanglement limit of *e* - 1 and then extend that bin
   packing to a packing of the full collection of packages.

3. If Step 2 fails, the packing we first found is
   minimal, so we are done.

4. Otherwise, we have found a better packing with a score
   *e'*, so start again at Step 2 with *e* = *e'*.

With this strategy in place (after a little grief), the
solution runs in about 50 milliseconds for Part A and 20
milliseconds for Part B. Note that Part B went from being 7x
slower to 3x faster than Part A: there are far fewer valid
packings for a smaller bin size. Part B ended up with a
speedup of about 20,000x.

This solution makes major use of laziness. The Haskell
notionally produces a complete list of all possible packings
(including duplicates because of symmetry).  However, only a
small portion of this space is ever examined, and this is
the only portion that is generated. One would have to write
in a substantially different style in a strict language, and
the resulting code would be much less versatile unless
engineering carefully.

I had a couple of small bugs, but for the most part the
first code I typed in for this worked. The type system was a
huge help here, as was the functional programming
decomposition of cases.

The `loop` function invoked here is something I stuck in
`Soln`: it iterates an `Either` monad on its `Right` output
until it finally returns `Left`, at which point this value
becomes the result.
