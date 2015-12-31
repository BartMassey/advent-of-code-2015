-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Soln

-- | Map from molecule (don't ask) to list of molecules.
type TransMap = M.Map [String] [[String]]

-- | Insert a new molecule in the list of productions for
-- a given molecule.
insertMulti :: [String] -> [String] -> TransMap -> TransMap
insertMulti k v m =
    M.insertWith (\[new] old -> new : old) k [v] m

-- | Read a molecule.
parseElem :: [String] -> String -> ([String], String)
parseElem cs (s1 : s2 : ss) | isUpper s1 && isLower s2 =
    (cs ++ [[s1, s2]], ss)
parseElem cs (s : ss) | isUpper s =
    (cs ++ [[s]], ss)
parseElem _ _ = error "bad elem"

-- | Read a rule.
parseRule :: [String] -> TransMap -> TransMap
parseRule [src, "=>", dst] m =
    let es = envelop parseElem [] dst in
    insertMulti [src] es m
parseRule _ _ = error "bad rule"

-- | Read a machine specification, both rules and input.
parseMachine :: String -> (TransMap, [String])
parseMachine stuff =
    let desc = lines stuff
        rules = take (length desc - 2) desc
        m = foldr (parseRule . words) M.empty rules
        elems = envelop parseElem [] $ last desc
    in
    (m, elems)

-- | Expansions of a given molecule starting at a given
-- split point.
expandSplit :: TransMap -> ([String], [String]) -> S.Set [String]
       -> S.Set [String]
expandSplit _ (_, []) _ = error "bad expansion"
expandSplit m (first, rest) result =
    M.foldrWithKey' expandOne result m
    where
      expandOne :: [String] -> [[String]] -> S.Set [String] -> S.Set [String]
      expandOne k vs a
          | isPrefixOf k rest =
              foldr (S.insert . splice) a vs
          | otherwise = a
          where
            splice :: [String] -> [String]
            splice v = first ++ v ++ drop (length k) rest

-- | Expansions of a given set of molecules.
expand :: TransMap -> S.Set [String] -> S.Set [String]
expand m s0 =
    S.foldr' oneItem S.empty s0
    where
      oneItem es s =
          foldr (expandSplit m) (s `S.difference` s0) $
            map (flip splitAt es) $ [0 .. length es - 1]

solna :: String -> IO ()
solna stuff = do
  let (m, es) = parseMachine stuff
  print $ S.size $ expand m $ S.singleton es

-- | Solution from
-- https://www.reddit.com/r/adventofcode/
--   comments/3xflz8/day_19_solutions/cy4h7ji
solnb :: String -> IO ()
solnb stuff = do
  let (_, es) = parseMachine stuff
  let n = length es
  let c s = length $ filter (== s) es
  print $ n - c "Rn" - c "Ar" - 2 * c "Y" - 1

main :: IO ()
main = makeMain solna solnb
