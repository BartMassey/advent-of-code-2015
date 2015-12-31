-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

{-# LANGUAGE FlexibleContexts #-}

import Data.Array.IO

import Soln

-- | Each dimension goes from 0 to 999 inclusive.
bound :: Int
bound = 999

-- | Every coordinate in the array, in y-major order.
allCoords :: [(Int, Int)]
allCoords = [(x, y) | x <- [0..bound], y <- [0..bound]]

-- | A mutable unboxed 2D array of lights.
type LightArray a = IOUArray (Int, Int) a

-- | Take a coordinate string to a coordinate pair.
parseCoords :: String -> (Int, Int)
parseCoords s =
    let (c1, c2) = break (== ',') s in
    (read c1, read (tail c2))

-- | Given a current state, an operator to apply,
-- and a string describing a rectangle, operate
-- on the affected lights.
operateLights :: MArray IOUArray a IO =>
             LightArray a -> (a -> a) -> [String] -> IO ()
operateLights lights op [c1Str, "through", c2Str] = do
    let (x1, y1) = parseCoords c1Str
    let (x2, y2) = parseCoords c2Str
    mapM_ lightOp [(x, y) | x <- [x1 `min` x2..x1 `max` x2],
                            y <- [y1 `min` y2..y1 `max` y2] ]
    where
      lightOp coord = do
        b <- readArray lights coord
        writeArray lights coord (op b)
        return ()
operateLights _ _ _ = error "bad coords"

-- | Uninterpreted lighting commands.
data Op = OpOff | OpOn | OpToggle

-- | English interpretation of lighting commands.
opsEnglish :: Op -> (Bool -> Bool)
opsEnglish OpOff = const False
opsEnglish OpOn = const True
opsEnglish OpToggle = not

-- | Elvish interpretation of lighting commands.
opsElvish :: Op -> (Int -> Int)
opsElvish OpOff x = 0 `max` (x - 1)
opsElvish OpOn x = x + 1
opsElvish OpToggle x = x + 2

-- | Given a command string, operate the lights
-- according to the given interpretation.
runCommand :: MArray IOUArray a IO =>
           LightArray a -> (Op -> (a -> a)) -> [String] -> IO ()
runCommand lights ops ("turn" : "off" : coords) =
    operateLights lights (ops OpOff) coords
runCommand lights ops ("turn" : "on" : coords) =
    operateLights lights (ops OpOn) coords
runCommand lights ops ("toggle" : coords) =
    operateLights lights (ops OpToggle) coords
runCommand _ _ _ = error "bad command"

-- | Strategy: Initialize the array as indicated, then run all
-- the commands according to the given interpretation and
-- process the resulting array elements as indicated
soln :: MArray IOUArray a IO =>
        String -> a -> (Op -> a -> a) -> ([a] -> Int) -> IO ()
soln stuff initialValue interp summarize = do
  lights <- newArray ((0, 0), (bound, bound)) initialValue
  mapM_ (runCommand lights interp) $ map words $ lines stuff
  ts <- mapM (readArray lights) allCoords
  print $ summarize ts

solna :: String -> IO ()
solna stuff =
    soln stuff False opsEnglish (length . filter id)

solnb :: String -> IO ()
solnb stuff =
    soln stuff 0 opsElvish sum

main :: IO ()
main = makeMain solna solnb
