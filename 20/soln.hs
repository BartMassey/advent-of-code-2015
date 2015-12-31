-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Factor
import Soln

-- | Number of presents at house 'i' in part A.
nPresents :: Int -> Int
nPresents i =
    10 * sum (allFactors i)

-- | Strategy: Keep checking houses.
solna :: String -> IO ()
solna stuff = do
  let t = read stuff
  let enoughPresents n = t <= nPresents n
  print $ fromJust $ find enoughPresents $ [1..]

-- | Number of presents at house 'i' in part B.
nPresents' :: Int -> Int
nPresents' i =
    11 * (sum $ filter stillElfing $ allFactors i)
    where
      stillElfing f =
          i `div` f <= 50

-- | Strategy: Keep checking houses.
solnb :: String -> IO ()
solnb stuff = do
  let t = read stuff
  let enoughPresents n = t <= nPresents' n
  print $ fromJust $ find enoughPresents $ [1..]

main :: IO ()
main = makeMain solna solnb
