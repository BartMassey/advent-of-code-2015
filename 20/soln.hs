-- Copyright © 2015 Bart Massey

import Factor
import Soln

nPresents :: Int -> Int
nPresents i =
    10 * sum (allFactors i)

solna :: String -> IO ()
solna stuff = do
  let t = read stuff
  let enoughPresents n = t <= nPresents n
  print $ fromJust $ find enoughPresents $ [1..]

nPresents' :: Int -> Int
nPresents' i =
    11 * (sum $ filter stillElfing $ allFactors i)
    where
      stillElfing f =
          i `div` f <= 50

solnb :: String -> IO ()
solnb stuff = do
  let t = read stuff
  let enoughPresents n = t <= nPresents' n
  print $ fromJust $ find enoughPresents $ [1..]

main :: IO ()
main = makeMain solna solnb
