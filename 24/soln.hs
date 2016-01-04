-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Soln

-- | Get the list of weights.
parseWeights :: String -> [Int]
parseWeights stuff =
    map read $ lines stuff

-- | A bin, with its current total weight tracked.
data Bin = Bin {
      binWeight :: Int,
      binPackages :: [Int] }

-- | Insert a package into a bin.
insertPackage :: Int -> Bin -> Bin
insertPackage p bin =
    Bin { binWeight = p + binWeight bin,
          binPackages = p : binPackages bin }

-- | The empty bin.
emptyBin :: Bin
emptyBin = Bin 0 []

-- | Pack up packages into bins. Return a list of packings
-- consisting of the target weight packing and the remaining
-- unpacked packages.
packBin :: Int -> [Int] -> [([Int], [Int])]
packBin targetWeight toPack =
    catMaybes $ map getSolution $
      foldr packPackage [(emptyBin, emptyBin)] toPack
    where
      totalWeight = sum toPack
      getSolution (packed, unpacked)
          | binWeight packed == targetWeight =
              Just (binPackages packed, binPackages unpacked)
          | otherwise = Nothing
      packPackage p partials =
          concatMap packPartial partials
          where
            packPartial (packed, unpacked)
                | p + packedWeight + remainingWeight < targetWeight = []
                | p + packedWeight > targetWeight = [dontPack]
                | otherwise = [doPack, dontPack]
                where
                  packedWeight = binWeight packed
                  remainingWeight = totalWeight - binWeight unpacked
                  doPack = (insertPackage p packed, unpacked)
                  dontPack = (packed, insertPackage p unpacked)


-- Partition a collection of packages into `n` bins, each with
-- the given target weight, in every way possible.
packBins :: Int -> Int -> [Int] -> [[[Int]]]
packBins n targetWeight packages =
    map complete $ (iterate packNext [([], packages)] !! (n - 1))
    where
      complete :: ([[Int]], [Int]) -> [[Int]]
      complete (partial, remaining) =
          remaining : partial
      packNext :: [([[Int]], [Int])] -> [([[Int]], [Int])]
      packNext partials =
          concatMap packPartial partials
          where
            packPartial (packed, remaining) =
                map recordPacking $ packBin targetWeight remaining
                where
                  recordPacking (packing, remaining') =
                      (packing : packed, remaining')
                
-- | Find a packing with a least-entangled bin. Return the
-- entanglement of that bin.
leastEntanglement :: [[[Int]]] -> Integer
leastEntanglement packings =
    let scorePacking = minimum . map (product . map fromIntegral) in
    minimum $ map scorePacking packings

-- | Strategy: Brute force.
soln :: Int -> String -> [[Int]]
soln n stuff = do
  let packages = sort $ parseWeights stuff
  let totalWeight = sum packages
  let sectionWeight
          | totalWeight `mod` n == 0 = totalWeight `div` n
          | otherwise = error "bad parity"
  head $ packBins n sectionWeight packages

solna :: String -> IO ()
solna stuff = print $ soln 3 stuff

solnb :: String -> IO ()
solnb stuff = print $ soln 4 stuff

main :: IO ()
main = makeMain solna solnb
