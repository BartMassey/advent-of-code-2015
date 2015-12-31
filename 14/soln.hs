-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Soln

-- | For some reason, the race is over after 2503 seconds.
endTime :: Int
endTime = 2503

-- | Description of a reindeer.
data Reindeer = Reindeer {
      velocity, flightTime, restTime :: Int }

-- | Read a reindeer description.
parseReindeer :: [String] -> Reindeer
parseReindeer [ _, "can", "fly", vs, "km/s", "for", tfs, "seconds,",
                "but", "then", "must", "rest", "for", trs, "seconds." ] =
    Reindeer (read vs) (read tfs) (read trs)
parseReindeer _ = error "bad reindeer"

-- | Produce a trace that shows the given reindeer's position
-- at every race second.
runReindeer :: Reindeer -> [Int]
runReindeer reindeer =
    concatMap makeLeg $ iterate legEnd 0
    where
      -- | Compute the position of the reindeer at the end of
      -- the leg.
      legEnd :: Int -> Int
      legEnd start =
          start + flightTime reindeer * velocity reindeer
      -- | Compute the position of the reindeer at each second
      -- of the leg.
      makeLeg :: Int -> [Int]
      makeLeg start =
          let end = legEnd start in
          [start, start + velocity reindeer ..
           end - velocity reindeer] ++
          replicate (restTime reindeer) end

-- | Produce traces for all reindeer in the race.
processReindeers:: String -> [[Int]]
processReindeers stuff =
    map (runReindeer . parseReindeer . words) $ lines stuff

-- | Strategy: Find a largest distance at end of race.
solna :: String -> IO ()
solna stuff = do
  print $ maximum $ map (!! endTime) $ processReindeers stuff

-- | Given the current position of each reindeer,
-- compute the score of each reindeer.
scorePosns :: [Int] -> [Int]
scorePosns posns =
    map (deltaMax (maximum posns)) posns
    where
      deltaMax maxPosn posn
          | posn == maxPosn = 1
          | otherwise = 0

-- | Strategy: Score each reindeer at each second, then
-- sum each reindeer's scores over the race to get an
-- overall score for that reindeer.  Return the maximum
-- total score.
solnb :: String -> IO ()
solnb stuff = do
  print $ maximum $ map sum $ transpose $ take endTime $ tail $
    map scorePosns $ transpose $ processReindeers stuff

main :: IO ()
main = makeMain solna solnb
