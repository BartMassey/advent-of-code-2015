-- Copyright © 2015 Bart Massey

import qualified Data.Map.Strict as M

import Soln

type TransMap = M.Map String [[String]]

insertMulti :: String -> [String] -> TransMap -> TransMap
insertMulti k v m =
    M.insertWith (\[new] old -> new : old) k [v] m

parseElem :: [String] -> String -> ([String], String)
parseElem cs (s1 : s2 : ss) | isUpper s1 && isLower s2 =
    (cs ++ [[s1, s2]], ss)
parseElem cs (s : ss) | isUpper s =
    (cs ++ [[s]], ss)
parseElem _ _ = error "bad elem"

parseRule :: [String] -> TransMap -> TransMap
parseRule [src, "=>", dst] m =
    let es = envelop parseElem [] dst in
    insertMulti src es m
parseRule _ _ = error "bad rule"

parseMachine :: String -> (TransMap, [String])
parseMachine stuff =
    let desc = lines stuff
        rules = take (length desc - 2) desc
        m = foldr (parseRule . words) M.empty rules
        elems = envelop parseElem [] $ last desc
    in
    (m, elems)

solna :: String -> IO ()
solna _ = error "no solution for a here"

solnb :: String -> IO ()
solnb stuff = do
  let (_, es) = parseMachine stuff
  -- Solution blatantly stolen from
  -- https://www.reddit.com/r/adventofcode/
  --   comments/3xflz8/day_19_solutions/cy4h7ji
  let n = length es
  let c s = length $ filter (== s) es
  print $ n - c "Rn" - c "Ar" - 2 * c "Y" - 1

main :: IO ()
main = makeMain solna solnb
