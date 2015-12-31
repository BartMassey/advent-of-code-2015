This problem *looks* as easy as Problem 21. While it does
indeed reuse some code from 21, it's a much trickier problem
in practice.

The sequencing is crucial, and I am not even sure that the
sequencing in the accepted solution is what is described by
the fairly loose problem description. What I mean by this is
that spell effects and effect expiration must be applied in
exactly the right order at exactly the right time, or the
whole thing gets wrong answers.

There are two solutions here. `old-soln.hs` is what I
originally wrote to solve the problem. `soln.hs` is
the properly engineered version: it uses the `Either` monad
and better decomposition to remove the recursion and to
make the code actually readable and understandable; it
also removes a fiddly fudge factor that shouldn't have been
there and changes the runtime for Part A from many seconds
to instantaneous.

I spent many, many hours debugging both solutions. There is
some testing code here that was crucial to getting things
right.
