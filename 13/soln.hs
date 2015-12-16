-- Copyright © 2015 Bart Massey

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Soln

type PrefMap = M.Map (String, String) Int

parsePref :: [String] -> ((String, String), Int)
parsePref [p1, "would", adj, v, "happiness", "units",
           "by", "sitting", "next", "to", p2dot ] =
    ((p1, init p2dot), gainLose adj (read v))
    where
      gainLose "lose" n = -n
      gainLose "gain" n = n
      gainLose _ _ = error "bad adjective"
parsePref _ = error "bad preference"

parsePrefs :: String -> PrefMap
parsePrefs stuff = M.fromList $ map (parsePref . words) $ lines stuff

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

findPeople :: [(String, String)] -> [String]
findPeople pairs =
    S.toList $ foldl' persons S.empty pairs
    where
       persons s (p1, p2) = S.insert p1 $ S.insert p2 s

processPrefs :: String -> (PrefMap, [String])
processPrefs stuff =
    let prefMap = parsePrefs stuff in
    (prefMap, findPeople $ M.keys prefMap)

solna :: String -> IO ()
solna stuff = do
  let (prefMap, people) = processPrefs stuff
  print $ bestScore prefMap people

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
