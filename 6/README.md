This problem is mostly a command-processing exercise.

Haskell has reasonably efficient "mutable" arrays, albeit
with confusing syntax and semantics. This solution makes use
of them.

The split of interpretation and form makes good use of
functional programming tools: the result is code that could
be reused for a variety of similar problems.
