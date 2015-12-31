-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


module Factor
where

import Data.List

-- | Integer square root.
-- <https://wiki.haskell.org/Generic_number_type#squareRoot>
isqrt :: Int -> Int
isqrt 0 = 0
isqrt 1 = 1
isqrt n =
    let twopows = iterate square 2
        (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
        newtonStep x = div (x + div n x) 2
        iters = iterate newtonStep (isqrt (div n lowerN) * lowerRoot)
        isRoot r  =  square r <= n && n < square (r+1)
    in  head $ dropWhile (not . isRoot) iters
    where
      square x = x * x

-- | List of primes by trial division.
primes :: [Int]
primes = 2 : [ i | i <- [3..],
                   and [i `mod` j /= 0 |
                   j <- takeWhile (<= isqrt i) primes]]

-- | Prime factors.
factors :: Int -> [Int]
factors n | n < 2 = error "cannot factor"
factors n0 = f n0 primes where
    f _ [] = error "no factors"
    f n (c : cs)
      | n == c = [c]
      | n `mod` c == 0 = c : f (n `div` c) (c : cs)
      | otherwise = f n cs  

-- | All factors including composites.
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
