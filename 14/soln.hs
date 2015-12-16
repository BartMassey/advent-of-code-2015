-- Copyright © 2015 Bart Massey

import Soln

parseReindeer :: [String] -> (Int, Int, Int)
parseReindeer [ _, "can", "fly", vs, "km/s", "for", tfs, "seconds,",
                  "but", "then", "must", "rest", "for", trs, "seconds." ] =
    (read vs, read tfs, read trs)
parseReindeer _ = error "bad reindeer"

runReindeer :: (Int, Int, Int) -> [Int]
runReindeer (v, tf, tr) =
    leg 0
    where
      leg s =
          let endLeg = s + tf * v in
          [s, s + v .. endLeg - v] ++
          replicate tr endLeg ++
          leg endLeg

processReindeers:: String -> [[Int]]
processReindeers stuff =
    map (runReindeer . parseReindeer . words) $ lines stuff

solna :: String -> IO ()
solna stuff = do
  print $ maximum $ map (!! 2503) $ processReindeers stuff

scorePosns :: [Int] -> [Int]
scorePosns posns =
    map (deltaMax (maximum posns)) posns
    where
      deltaMax m p
          | m == p = 1
          | otherwise = 0

solnb :: String -> IO ()
solnb stuff = do
  print $ maximum $ map sum $ transpose $ take 2503 $ tail $
    map scorePosns $ transpose $ processReindeers stuff

main :: IO ()
main = makeMain solna solnb
