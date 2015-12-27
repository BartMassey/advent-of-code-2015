The key to Part B of this problem is to re-use the machinery
of Part A by using the fact that Santa and Robo-Santa are
completely separate entities.

This problem might better be done by keeping track of moves
using `Data.Set` rather than lists. This approach would
certainly be more efficient. In any case, the approach used
illustrates the use of the somewhat obscure
`Data.List.mapAccumL`.
