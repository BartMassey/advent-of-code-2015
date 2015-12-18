-- Copyright © 2015 Bart Massey

import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as M
import System.Console.ANSI
import System.IO (hFlush, stdout)

import Soln

data Light = LightOn | LightOff
           deriving Eq

type LightMap = M.Map (Int, Int) Light

nFrames :: Int
nFrames = 100

xSize, ySize :: Int
xSize = 50
ySize = 20

frameRate :: Int
frameRate = 10

indices :: [(Int, Int)]
indices = [(x, y) | y <- [1 .. ySize], x <- [1 .. xSize]]

readLights :: String -> LightMap
readLights stuff =
    M.fromList $ zip indices $ map readLight $ concat $ lines stuff
    where
      readLight '#' = LightOn
      readLight '.' = LightOff
      readLight _ = error "bad light"

countLights :: [(Int, Int)] -> LightMap -> Int
countLights ixs m = 
    sum $ map (lightValue . flip M.lookup m) ixs
    where
      lightValue (Just LightOn) = 1
      lightValue _ = 0

playLife :: LightMap -> LightMap
playLife m =
    M.mapWithKey updateLight m
    where
      updateLight (x, y) l
          | n < 2 = LightOff
          | n > 3 = LightOff
          | n == 3 = LightOn
          | n == 2 && l == LightOn = LightOn
          | otherwise = LightOff
          where
            n = countLights
                  [ (x + dx, y + dy) |
                    dy <- [-1 .. 1],
                    dx <- [-1 .. 1],
                    (dx, dy) /= (0, 0) ] m

stickCorners :: LightMap -> LightMap
stickCorners m0 =
    foldr stickCorner m0 [(x, y) | x <- [1, xSize], y <- [1, ySize]]
    where
      stickCorner xy m = M.insert xy LightOn m

soln :: (LightMap -> LightMap) -> String -> IO ()
soln nextFrame stuff = do
  mapM_ showFrame $ take nFrames makeFrames
  where
    makeFrames = iterate nextFrame $ readLights stuff
    showFrame m = do
      clearScreen
      setCursorPosition 0 0
      putStr $ unlines $ transpose $ tiles ySize ySize $
        map lightChar $ M.elems m
      hFlush stdout
      threadDelay $ 1000000 `div` frameRate
      where
        lightChar LightOn = '#'
        lightChar LightOff = '.'
                                        
solna :: String -> IO ()
solna stuff = do
  soln playLife stuff

solnb :: String -> IO ()
solnb stuff = do
  soln (stickCorners . playLife) stuff

main :: IO ()
main = makeMain solna solnb
