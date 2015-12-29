-- Copyright © 2015 Bart Massey

import qualified Data.Map as M

import Soln

-- | Map from characteristic name to characteristic value.
type Aunt = M.Map String Int

-- | Map from characteristic name to a function that
-- indicates whether the quantity of that ingredient is in
-- the acceptable range.
type AuntEval = M.Map String (Int -> Bool)

-- | The aunt of Part A.
targetAunt :: AuntEval
targetAunt = M.fromList [
              ("children:", (== 3)),
              ("cats:",  (== 7)),
              ("samoyeds:", (== 2)),
              ("pomeranians:", (== 3)),
              ("akitas:", (== 0)),
              ("vizslas:", (== 0)),
              ("goldfish:", (== 5)),
              ("trees:", (== 3)),
              ("cars:", (== 2)),
              ("perfumes:", (== 1)) ]

-- | The aunt of Part B.
targetAunt' :: AuntEval
targetAunt' = M.fromList [
              ("children:", (== 3)),
              ("cats:",  (> 7)),
              ("samoyeds:", (== 2)),
              ("pomeranians:", (< 3)),
              ("akitas:", (== 0)),
              ("vizslas:", (== 0)),
              ("goldfish:", (< 5)),
              ("trees:", (> 3)),
              ("cars:", (== 2)),
              ("perfumes:", (== 1)) ]

-- | Parse an aunt description.
parseAunt :: [String] -> (Int, Aunt)
parseAunt ("Sue" : nStr : attrs) =
    (read $ init nStr, foldr parseAttrs M.empty $ tiles 2 2 attrs)
    where
      parseAttrs [k, vStr] m =
          M.insert k (read $ filter isDigit $ vStr) m
      parseAttrs _ _ = error "bad attr"
parseAunt _ = error "bad aunt"
                     
-- | Get all the aunt descriptions.
parseAunts :: String -> [(Int, Aunt)]
parseAunts stuff =
    map (parseAunt . words) $ lines stuff

-- | Strategy: Look for an aunt that matches
-- the given target aunt.
soln :: AuntEval -> String -> IO ()
soln aTargetAunt stuff =
    case lookup True $ map auntTest $ parseAunts stuff of
      Just a -> print a
      Nothing -> error "no matching aunt"
    where
      auntTest (num, desc) =
          (all attrMatch (M.keys desc), num)
          where
            attrMatch k =
                case M.lookup k aTargetAunt <*> M.lookup k desc of
                  Just b -> b
                  Nothing -> error "invalid aunt"

solna :: String -> IO ()
solna stuff = do
  soln targetAunt stuff

solnb :: String -> IO ()
solnb stuff = do
  soln targetAunt' stuff

main :: IO ()
main = makeMain solna solnb
