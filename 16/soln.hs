-- Copyright © 2015 Bart Massey

import qualified Data.Map as M

import Soln

type Aunt = M.Map String Int
type AuntEval = M.Map String (Int -> Bool)

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

parseAunt :: [String] -> (Int, Aunt)
parseAunt ("Sue" : nStr : attrs) =
    (read $ init nStr, foldr parseAttrs M.empty $ tiles 2 2 attrs)
    where
      parseAttrs [k, vStr] m =
          M.insert k (read $ filter isDigit $ vStr) m
      parseAttrs _ _ = error "bad attr"
parseAunt _ = error "bad aunt"
                     
parseAunts :: String -> [(Int, Aunt)]
parseAunts stuff =
    map (parseAunt . words) $ lines stuff

soln :: AuntEval -> String -> IO ()
soln aTargetAunt stuff =
    case find correctAunt $ parseAunts stuff of
      Just (a, _) -> print a
      Nothing -> error "no matching aunt"
    where
      correctAunt (_, aunt) =
          all attrMatch $ M.keys aunt
          where
            attrMatch k =
                case M.lookup k aTargetAunt <*> M.lookup k aunt of
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
