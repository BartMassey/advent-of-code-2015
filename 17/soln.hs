-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Soln

-- | Amount of eggnog to be stored.
targetVolume :: Int
targetVolume = 150

-- | All ways the target volume can be achieved.
solns :: String -> [[Int]]
solns stuff =
    filter ((== targetVolume) . sum) $ subsequences $ map read $ lines stuff

solna :: String -> IO ()
solna stuff = do
  print $ length $ solns stuff

-- | Strategy: find the minimum number of containers, then
-- count the number of fillings using that many.
solnb :: String -> IO ()
solnb stuff = do
  let minContainers = minimum $ map length $ solns stuff
  print $ length $ filter ((== minContainers) . length) $ solns stuff

main :: IO ()
main = makeMain solna solnb
