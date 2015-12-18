-- Copyright © 2015 Bart Massey

import qualified Data.Map as M

import Soln

type LightMap = M.Map (Int, Int) Int

indices :: [(Int, Int)]
indices = [(x, y) | y <- [0..99], x <- [0..99]]

readLights :: String -> LightMap
readLights stuff =
    M.fromList $ zip indices $ map readLight $ concat $ lines stuff
    where
      readLight '#' = 1
      readLight '.' = 0
      readLight _ = error "bad light"

nextFrame :: LightMap -> LightMap
nextFrame m =
    M.fromList $ map (\xy -> (xy, updateLight xy)) indices
    where
      updateLight xy  =
          let neighbors =
                sum $ catMaybes $ findNeighbors xy in
          case neighbors of
            x | x < 2 -> 0
            x | x > 3 -> 0
            x | x == 3 -> 1
            x | x == 2 && Just 1 == M.lookup xy m -> 1
            _ -> 0 
          where
            findNeighbors (x, y) =
                map (flip M.lookup m) [(x + dx, y + dy) |
                                       dx <- [-1 .. 1],
                                       dy <- [-1 .. 1],
                                       (dx, dy) /= (0, 0)]

stickCorners :: LightMap -> LightMap
stickCorners m =
    foldr (\xy m' -> M.insert xy 1 m') m [(0,0),(0,99),(99,0),(99,99)]

countLights :: LightMap -> Int
countLights m =
    sum $ catMaybes $ map (flip M.lookup m) indices

solna :: String -> IO ()
solna stuff = do
  print $ countLights $ (iterate nextFrame $ readLights stuff) !! 100

solnb :: String -> IO ()
solnb stuff = do
  print $ countLights $ (iterate (stickCorners . nextFrame) $ stickCorners $ readLights stuff) !! 100

main :: IO ()
main = makeMain solna solnb
