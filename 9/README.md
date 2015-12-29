This is a good example of a problem for which way more
efficient solutions (by large constant factors) than brute
force are available, but for which brute force is good
enough.

The Haskell solution makes use of `tile` from `Soln` to
compute distances of paths. An alternative would be to
`foldr` over a tuple containing the current location and the
accumulated distance.
