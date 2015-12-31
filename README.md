# Advent Of Code 2015: Tutorial Solutions in Haskell
Copyright (c) 2015 Bart Massey

Herein lie solutions to all of the problems of the 2015
[Advent of Code](http://adventofcode.com). Advent of Code
was a fantastic exercise, and I thank the author and others
involved profusely for their excellent work. Thanks also to
`relsqui` for pointing me at this on about Day 9. I had some
catching up to do.

For each solution, I have included commented and cleaned-up
Haskell code. The solution file will always be `soln.hs`,
and the `Makefile` will compile it to `soln` using
[GHC](http://www.haskell.org/ghc/).  There is a `README.md`
in every problem directory containing descriptions and
comments. I have also included the problem descriptions
(`problem-a.md` and `problem-b.md`) and my specific
`input.txt` for posterity.

I assume you have GHC running on a fast-ish UNIX box with a
bunch of memory (although most everything should also work
on other operating systems).  For a few problems you will
also need to install extra packages from
[Hackage](http://hackage.haskell.org). The easiest way to do
that is with
`[cabal](https://wiki.haskell.org/Cabal-Install)` AKA
`cabal-install`, so you will want to learn how to operate
that.

The goals of these solutions are to:

* Provide canonical correct solutions with reasonable
  runtimes.

* Illustrate reasonable solution strategies.

* Illustrate the use of Haskell in problem-solving,
  including some "advanced" techniques that aren't really
  advanced and should be part of every Haskell programmer's
  repertoire.

I learned a ton of Haskell and a little bit of software
engineering I should already have known writing these.

These solutions deserve a much more thorough top-level
description than I have the energy to write at this point.
I will revise this file in the indefinite future.

I am under no illusions that I am a superior Haskell
programmer.  Indeed, I suspect certain members of the
Haskell community will be more amused than impressed by my
fairly straightforward solutions. Feedback and pull requests
are extremely welcome! Let me know what I should have done,
and I'll try to make it right.

This work is licensed under the "MIT License".  Please see
the file `COPYING` in the source distribution of this software
for license terms.

