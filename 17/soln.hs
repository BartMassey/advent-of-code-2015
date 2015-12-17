-- Copyright © 2015 Bart Massey

import Soln

solna :: String -> IO ()

solns :: String -> [[Int]]
solns stuff =
    filter ((== 150) . sum) $ subsequences $ map read $ lines stuff

solna stuff = do
  print $ length $ solns stuff

solnb :: String -> IO ()
solnb stuff = do
  let candidateStuff = solns stuff
  let minContainers = minimum $ map length $ candidateStuff
  print $ length $ filter ((== minContainers) . length) $ candidateStuff

main :: IO ()
main = makeMain solna solnb
