-- Copyright © 2015 Bart Massey

import Soln

prng :: Int -> [Int]
prng seed =
    iterate next seed
    where
      next v = (252533 * v) `mod` 33554393

index :: (Int, Int) -> Int
index (r, c) = (r + c) * (r + c + 1) `div` 2 + c

solna :: String -> IO ()
solna _ = do
  print $ (prng 20151125) !! index (3009, 3018)

solnb :: String -> IO ()
solnb _ = error "no part b"

main :: IO ()
main = makeMain solna solnb
