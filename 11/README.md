This problem is eerily reminiscent of problem 5. Unlike 5,
though, it is a bit ill-specified: it is not clear which
"pairs of letters" count. Does "xaaax" have two, one or zero
pairs?

The Haskell here is pretty unremarkable. The hardest part of
writing it was perfecting `incrChar`, which does an
add-with-carry on a character of the string.
