Just build a machine simulator and run it. This could be a
speed-programming exercise, but I went for elegance.

The originally-published version of this code contained the
"[Existential Typeclass Antipattern](https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/)"
in a big way. Cleaning it up resulted in much smaller and
more readable code while sacrificing nothing much. Recursion
was also removed via `iterate`.

The simulator should be quite extensible for other kinds of
instructions and machines; I thought that might be important
for Part B, but it wasn't.
