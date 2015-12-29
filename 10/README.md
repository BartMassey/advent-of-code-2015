Pretty straightforward string-processing exercise.

The Haskell makes use of laziness to good effect in `grow`,
where `iterate` is used to generate an "infinite list" of
expansions, but only the n-th one is returned.
