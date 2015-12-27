There is no way to do this one but by brute force, I think.

This is one of the few cases in which a Hackage package is a
more-or-less essential part of the solution. Install
`cryptohash` from Hackage to get MD5.

Some care has been taken to avoid Unicode issues. Haskell's
`Char` datatype is Unicode, as it should be. So we use
`Data.ByteString` for the hashes themselves, and
`Data.ByteString.Char8` only for the input string. The
result is that this code should work fine with an ISO-8801
input password, but will probably work not so good for
Unicode. One could fix this fairly straightforwardly to
support Unicode passwords, but this seemed like an odd thing
to do.

This code is reasonably efficient, printing the Part B
answer in about 1.6s. It could be made more efficient by
working directly with the MD5 output and the input
`ByteString` rather than a bunch of conversions.
