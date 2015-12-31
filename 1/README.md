This problem is pretty clearly intended as a warmup, and
it's a nice one: gets us used to the setup and whatnot.

This is pretty straightforward Haskell, but does nicely show
the power of folds. The PartB solution uses the `Either`
monad as an early termination mechanism. I really wanted to
use the identifiers `traverse` and `floor`, so I blocked
them out of the `Prelude` to avoid warnings.
