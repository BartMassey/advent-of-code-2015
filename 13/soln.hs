-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Soln

-- | Map (directed) pairs of people to score.
type PrefMap = M.Map (String, String) Int


-- | Parse an individual preference.
parsePref :: [String] -> ((String, String), Int)
parsePref [p1, "would", adj, v, "happiness", "units",
           "by", "sitting", "next", "to", p2dot ] =
    ((p1, init p2dot), gainLose adj (read v))
    where
      gainLose "lose" n = -n
      gainLose "gain" n = n
      gainLose _ _ = error "bad adjective"
parsePref _ = error "bad preference"

-- | Parse a preference list into a 'PrefMap'.
parsePrefs :: String -> PrefMap
parsePrefs stuff = M.fromList $ map (parsePref . words) $ lines stuff

-- | List of all people in the map.
findPeople :: [(String, String)] -> [String]
findPeople pairs =
    S.toList $ foldl' persons S.empty pairs
    where
       persons s (p1, p2) = S.insert p1 $ S.insert p2 s

-- | Get parse of preferences in final form.
processPrefs :: String -> (PrefMap, [String])
processPrefs stuff =
    let prefMap = parsePrefs stuff in
    (prefMap, findPeople $ M.keys prefMap)

-- | Strategy: Brute force.
bestScore :: PrefMap -> [String] -> Int
bestScore _ [] = 0
bestScore m xs =
    maximum $ map scorePerm $ permutations xs
    where
      scorePerm [] = 0
      scorePerm ps@(p : _) =
          sum $ map scorePair $ concatMap permutations $ tiles 2 1 $ ps ++ [p]
          where
            scorePair [p1, p2] = fromJust $ M.lookup (p1, p2) m
            scorePair _ = error "bad tile"

solna :: String -> IO ()
solna stuff = do
  let (prefMap, people) = processPrefs stuff
  print $ bestScore prefMap people

-- | Strategy: Hack up the input, then go.
solnb :: String -> IO ()
solnb stuff = do
  let (prefMap, people) = processPrefs stuff
  let prefMap' = foldl' addMe prefMap people
                 where
                   addMe m p =
                       M.insert ("me", p) 0 $ M.insert (p, "me") 0 m
  let people' = "me" : people
  print $ bestScore prefMap' people'

main :: IO ()
main = makeMain solna solnb
