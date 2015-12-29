There are probably more efficient plans than making a trace
of the position of each reindeer at each second of the race.
However, linear algebra is hard.

The Haskell code uses `iterate` to generate an "infinite"
trace for each reindeer. `transpose` is used twice in the
Part B scoring: once to get all the positions at a given
second, and then again to turn it back into a score trace.
