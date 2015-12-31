-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Soln

-- | Get the list of weights.
parseWeights :: String -> [Int]
parseWeights stuff =
    map read $ lines stuff

-- | Generate all the asymmetric splits of the first few
-- elements. This removes some symmetries from the overall
-- problem.
asymmetricSplits :: Int -> Int -> [Int] -> [[[Int]]]
asymmetricSplits n _ [] = [replicate n []]
asymmetricSplits n maxWeight (w : ws) = nub $ do
  partial <- asymmetricSplits n maxWeight ws
  (first, bin : rest) <- splits partial
  let newWeight = sum bin + w
  guard $ newWeight <= maxWeight
  guard $ all (newWeight <=) (map sum rest)
  return (first ++ [w : bin] ++ rest)

-- | Generate all the valid splits.
allSplits :: Int -> Int -> [Int] -> [[[Int]]]
allSplits n maxWeight ws | length ws <= n =
    asymmetricSplits n maxWeight ws
allSplits _ _ [] = error "unobtainium"
allSplits n maxWeight (w : ws) = do
  partial <- partials
  (first, bin : rest) <- splits partial
  guard $ sum bin + w <= maxWeight
  return (first ++ [w : bin] ++ rest)
  where
    partials = allSplits n maxWeight ws

-- | Strategy: Brute force.
soln :: Int -> String -> IO ()
soln n stuff = do
  let weights = sort $ parseWeights stuff
  let totalWeight = sum weights
  let sectionWeight
          | totalWeight `mod` n == 0 = totalWeight `div` n
          | otherwise = error "bad parity"
  let packings = allSplits n sectionWeight weights
  print $ best packings
  where
    best :: [[[Int]]] -> Integer
    best packings =
        let scorePacking = minimum . map (product . map fromIntegral)
            scores = map scorePacking packings
        in
        minimum scores

solna :: String -> IO ()
solna stuff = soln 3 stuff

solnb :: String -> IO ()
solnb stuff = soln 4 stuff

main :: IO ()
main = makeMain solna solnb
