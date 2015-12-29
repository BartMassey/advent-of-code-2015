Interesting logic puzzle, although hard-coding the target
aunt's description makes the problem a little weird.

Haskell makes good use of lists of functions here, to
evaluate the descriptors. Note the use of `<*>` in `soln` to
avoid some `Maybe`-whacking. `soln` still seems overly
complicated, though: probably can do something to make it
cleaner.
