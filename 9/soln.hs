-- Copyright © 2015 Bart Massey

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Soln

type DistMap = M.Map (String, String) Int

parseRoute :: [String] -> (String, String, Int)
parseRoute [c1, "to", c2, "=", ds] = (c1, c2, read ds)
parseRoute _ = error "bad route"

parseMap :: String -> ([String], DistMap)
parseMap stuff =
    let triples = map parseRoute $ map words $ lines stuff in
    let cities = S.toList $ foldl' findCities S.empty triples in
    let distMap = foldl' insertRoute M.empty triples in
    (cities, distMap)
    where
      insertRoute distMap (c1, c2, d) =
          M.insert (c1, c2) d $ M.insert (c2, c1) d distMap
      findCities citySet (c1, c2, _) =
          S.insert c1 $ S.insert c2 citySet


dist :: DistMap -> [String] -> Int
dist distMap route =
    sum $ map legDist $ tiles 2 1 route
    where
      legDist [c1, c2] =
          case M.lookup (c1, c2) distMap of
            Just d -> d
            Nothing -> error "bad leg"
      legDist _ = error "internal error: leg length"

-- XXX This scales *horribly*

type MByType a = (a -> a -> Ordering) -> [a] -> a

soln :: MByType [String] -> String -> IO ()
soln mBy stuff = do
  let (cities, distMap) = parseMap stuff
  let optRoute = mBy (comparing (dist distMap)) $ permutations cities
  print $ dist distMap optRoute

solna :: String -> IO ()
solna stuff = soln minimumBy stuff

solnb :: String -> IO ()
solnb stuff = soln maximumBy stuff

main :: IO ()
main = makeMain solna solnb
