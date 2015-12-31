Just build a machine simulator and run it. This could be a
speed-programming exercise, but I went for elegance.

This Haskell solution makes good use of a typeclass to allow
instructions with different operand formats to be treated
generically. This requires the use of an existential
quantifier (`forall` LOL) and an extra datatype to use a
list of generic instructions. The resulting simulator should
be quite extensible for other kinds of instructions and
machines; I thought that might be important for Part B, but
it wasn't.
