--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Factor
where

import Data.List

isqrt :: Int -> Int
isqrt n = (floor . (sqrt :: Double -> Double) . fromIntegral) n

primes :: [Int]
primes = 2 : [ i | i <- [3..],
                   and [i `mod` j /= 0 |
                   j <- takeWhile (<= isqrt i) primes]]

factors :: Int -> [Int]
factors 1 = [1]
factors n0 = f n0 primes where
    f _ [] = error "no factors"
    f n (c : cs) = if n == c then [c] else
                       if n `mod` c == 0 then c : f (n `div` c) (c : cs)
                       else f n cs  

allFactors :: Int -> [Int]
allFactors n =
    processMultiples $ group $ factors n
    where
      multiples :: [Int] -> [Int]
      multiples g = map product $ inits g
      processMultiples :: [[Int]] -> [Int]
      processMultiples [] = []
      processMultiples [g] =
          multiples g
      processMultiples (g : gs) =
          let ms = multiples g
              fs = processMultiples gs in
          [ m * f | f <- fs, m <- ms ]
