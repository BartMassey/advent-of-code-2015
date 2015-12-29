Cool. A little RPG simulator. Probably easiest to just
brute force it as usual.

This Haskell solution makes good use of `do` notation in the
list monad to generate all possible equipped PCs. The item
table should probably be read from a file instead of being
hard-coded.
