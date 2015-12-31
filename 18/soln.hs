-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import qualified Data.Map.Strict as M

import Soln

-- | We use explicit light states for type safety.
data Light = LightOn | LightOff

-- | State of the board.
type LightMap = M.Map (Int, Int) Light

-- | Number of turns the run is supposed to be.
nFrames :: Int
nFrames = 100

-- | Dimensions of the grid.
xSize, ySize :: Int
xSize = 100
ySize = 100

-- | List of all grid positions in row-major order.
indices :: [(Int, Int)]
indices = [(x, y) | y <- [1 .. ySize], x <- [1 .. xSize]]

-- | Read a board state.
readLights :: String -> LightMap
readLights stuff =
    M.fromList $ zip indices $ map readLight $ concat $ lines stuff
    where
      readLight '#' = LightOn
      readLight '.' = LightOff
      readLight _ = error "bad light"


-- | Count the number of turned-on lights in portion of
-- the given indices that are actually in the light map.
countLights :: [(Int, Int)] -> LightMap -> Int
countLights ixs m = 
    sum $ map (lightValue . flip M.lookup m) ixs
    where
      lightValue (Just LightOn) = 1
      lightValue _ = 0

-- | One round of the Game of Life with Christmas lights.
playLife :: LightMap -> LightMap
playLife m =
    M.mapWithKey updateLight m
    where
      updateLight (x, y) l
          | n < 2 = LightOff
          | n > 3 = LightOff
          | n == 3 = LightOn
          | n == 2 = l
          | otherwise = LightOff
          where
            n = countLights
                  [ (x + dx, y + dy) |
                    dy <- [-1 .. 1],
                    dx <- [-1 .. 1],
                    (dx, dy) /= (0, 0) ] m

-- | Turn any off corner lights back on.
stickCorners :: LightMap -> LightMap
stickCorners m0 =
    foldr stickCorner m0 [(x, y) | x <- [1, xSize], y <- [1, ySize]]
    where
      stickCorner xy m = M.insert xy LightOn m

-- | Strategy: Run the given transformation forward to the end
-- and print the resulting light count.
soln :: (LightMap -> LightMap) -> String -> IO ()
soln nextFrame stuff = do
  print $ countLights indices $ makeFrames !! nFrames
  where
    makeFrames = iterate nextFrame $ readLights stuff

solna :: String -> IO ()
solna stuff = do
  soln playLife stuff

solnb :: String -> IO ()
solnb stuff = do
  soln (stickCorners . playLife) stuff

main :: IO ()
main = makeMain solna solnb
