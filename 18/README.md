OK, Game of Life. Probably those who did not have an
implementation lying around missed the leaderboard.

This Haskell implementation uses `Data.Map` to store the
lights. This is probably less efficient than using an array,
but it's fast enough. Haskell arrays are also kind of a
pain. Also, this solution allows avoiding the usual clipping
problems at board edges altogether without padding the array
due to the magic of `Maybe`.

I've included a solution that shows Life playing out on a
smaller board. It requires the `ansi-terminal` and
`unbounded-delay` packages from Hackage, and gives a hint of
what can be done with interactive real-time stuff in
Haskell.
