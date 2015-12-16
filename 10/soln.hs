-- Copyright © 2015 Bart Massey

import Soln

say :: String -> String
say s =
    concatMap sayGroup $ group s
    where
      sayGroup ds = show (length ds) ++ [head ds]

grow :: Int -> String -> String
grow n s = iterate say s !! n

soln :: Int -> String -> IO ()
soln n stuff = print $ length $ grow n $ head $ words stuff

solna :: String -> IO ()
solna stuff = do
  soln 40 stuff

solnb :: String -> IO ()
solnb stuff = do
  soln 50 stuff

main :: IO ()
main = makeMain solna solnb
