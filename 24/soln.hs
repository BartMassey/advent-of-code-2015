-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Soln

-- | Get the list of weights.
parseWeights :: String -> [Int]
parseWeights stuff =
    map read $ lines stuff

-- | A bin, with its current total weight and entanglement tracked.
data Bin = Bin {
      binWeight :: Int,
      binEntanglement :: Integer,
      binPackages :: [Int] }

-- | Insert a package into a bin.
insertPackage :: Int -> Bin -> Bin
insertPackage p bin =
    Bin { binWeight = p + binWeight bin,
          binEntanglement = fromIntegral p * binEntanglement bin,
          binPackages = p : binPackages bin }

-- | The empty bin.
emptyBin :: Bin
emptyBin = Bin 0 1 []

-- | Pack up packages into bins. Return a list of packings
-- consisting of the target weight packing and the remaining
-- unpacked packages.
packBin :: Maybe Integer -> Int -> [Int] -> [([Int], [Int])]
packBin maxEntanglement targetWeight toPack =
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
                | excessiveEntanglement = [dontPack]
                | p + packedWeight > targetWeight = [dontPack]
                | otherwise = [doPack, dontPack]
                where
                  excessiveEntanglement =
                      case maxEntanglement of
                        Nothing -> False
                        Just e -> fromIntegral p * binEntanglement packed > e
                  packedWeight = binWeight packed
                  remainingWeight = totalWeight - binWeight unpacked
                  doPack = (insertPackage p packed, unpacked)
                  dontPack = (packed, insertPackage p unpacked)

-- | Partition a collection of packages into `n` bins, each
-- with equal weight, in every way possible. Constrain the
-- packings so that at least one has limited entanglement.
packBins :: Maybe Integer -> Int -> [Int] -> [[[Int]]]
packBins maxEntanglement n packages =
    map complete $ (iterate (packNext Nothing) oneBin !! (n - 2))
    where
      totalWeight = sum packages
      targetWeight
          | totalWeight `mod` n == 0 = totalWeight `div` n
          | otherwise = error "bad package parity"
      oneBin = packNext maxEntanglement [([], packages)]
      complete :: ([[Int]], [Int]) -> [[Int]]
      complete (partial, remaining) =
          remaining : partial
      packNext :: Maybe Integer -> [([[Int]], [Int])] -> [([[Int]], [Int])]
      packNext maxBin partials =
          concatMap packPartial partials
          where
            packPartial (packed, remaining) =
                map recordPacking $ packBin maxBin targetWeight remaining
                where
                  recordPacking (packing, remaining') =
                      (packing : packed, remaining')

-- | Return a least-entanglement packing of the given
-- packages. Strategy: keep trying packings with smaller
-- scores until no possible packings remain. The last score
-- must be minimal, by construction.
bestPacking :: Int -> [Int] -> [[Int]]
bestPacking n packages =
    loop improvePacking (undefined, Nothing)
    where
      improvePacking :: ([[Int]], Maybe Integer)
                     -> Either [[Int]] ([[Int]], Maybe Integer)
      improvePacking (bestSoln, bestEntanglement) =
          case packBins (fmap (subtract 1) bestEntanglement) n packages of
            [] -> Left bestSoln
            (packing : _ ) -> Right (packing, Just (scorePacking packing))

-- | Return the entanglement of a bin. Note that this will in
-- general be a product too big to fit in an 'Int'.
entanglement :: [Int] -> Integer
entanglement bin =
    product $ map fromIntegral bin

-- | Return the entanglement of the least-entangled
-- bin in a packing.
scorePacking :: [[Int]] -> Integer
scorePacking packing =
    minimum $ map entanglement packing

soln :: Int -> String -> [[Int]]
soln n stuff = do
  let packages = sort $ parseWeights stuff
  bestPacking n packages

solna :: String -> IO ()
solna stuff = print $ scorePacking $ soln 3 stuff

solnb :: String -> IO ()
solnb stuff = print $ scorePacking $ soln 4 stuff

main :: IO ()
main = makeMain solna solnb
