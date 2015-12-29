This is just a list-processing exercise. It is probably
better to just count characters rather than
unescaping/escaping strings and taking lengths. One could
use the built-in Haskell `read` and `show`, but this is
fraught with error potential.

The only interesting Haskell here is treating strings as
lists, and the use of the `envelop` fold from `Soln`.
