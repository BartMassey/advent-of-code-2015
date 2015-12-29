Part A of this problem looks like JSON is a red herring. You
can simply strip out all the things that look like numbers
and add them up. Sadly, after doing so, you find that Part B
pretty much requires parsing the JSON anyhow.

I learned how to use three JSON parsers for this. The
standard is Aeson, which worked fine in the end, but
initially I had trouble figuring out how to use it in this
simple "just give me a parse without a schema" problem. I
got things working with Yocto, then Json, then Aeson. It
turned out to be just a few lines of code in each case, but
they were hard to get to because of weak documentation and
my weak understanding of Haskell.

I used a recursive solution: using a built-in parser for
trees would be more appropriate, but I didn't have anything
lying around that I could figure out how to work with the
JSON datatypes I had.

One interesting problem that all solutions shared is what to
do with the JSON specification's loose notion of numbers.
Technically, all JSON numbers are rationals, represented in
decimal format. The various Haskells interpret this in a
variety of ways, one of which is not to split up 'Integer'
and non-'Integer' numbers in the JSON, nor to just represent
the rationals as 'Double'. In the end, I just chose to
convert sums to double and print them at the end.
