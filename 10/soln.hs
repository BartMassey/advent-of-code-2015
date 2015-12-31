-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Soln

-- | Compute the "say-string" of an input string.
say :: String -> String
say s =
    concatMap sayGroup $ group s
    where
      sayGroup ds = show (length ds) ++ [head ds]

-- | Compute the "say-string" resulting from `n` iterations
-- of `say` on the input string.
grow :: Int -> String -> String
grow n s = iterate say s !! n

-- | Strategy: Brute force
soln :: Int -> String -> IO ()
soln n stuff = print $ length $ grow n $ head $ lines stuff

solna :: String -> IO ()
solna stuff = do
  soln 40 stuff

solnb :: String -> IO ()
solnb stuff = do
  soln 50 stuff

main :: IO ()
main = makeMain solna solnb
