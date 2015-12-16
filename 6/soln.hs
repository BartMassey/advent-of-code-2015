{-# LANGUAGE FlexibleContexts #-}
-- Copyright © 2015 Bart Massey

import Control.Monad
import Data.Array.IO

import Soln

bound :: Int
bound = 999

type LightArray a = IOUArray (Int, Int) a

parseCoords :: String -> (Int, Int)
parseCoords s =
    let (c1, c2) = break (== ',') s in
    (read c1, read (tail c2))


runCoords :: MArray IOUArray a IO =>
             LightArray a -> (a -> a) -> [String] -> IO ()
runCoords lights op [c1Str, "through", c2Str] = do
    let (x1, y1) = parseCoords c1Str
    let (x2, y2) = parseCoords c2Str
    mapM_ lightOp [(x, y) | x <- [x1 `min` x2..x1 `max` x2],
                            y <- [y1 `min` y2..y1 `max` y2] ]
    where
      lightOp coord = do
        b <- readArray lights coord
        writeArray lights coord (op b)
        return ()
runCoords _ _ _ = error "bad coords"

data Op = OpOff | OpOn | OpToggle

opsEnglish :: Op -> (Bool -> Bool)
opsEnglish OpOff = const False
opsEnglish OpOn = const True
opsEnglish OpToggle = not

opsElvish :: Op -> (Int -> Int)
opsElvish OpOff x = 0 `max` (x - 1)
opsElvish OpOn x = x + 1
opsElvish OpToggle x = x + 2

process :: MArray IOUArray a IO =>
           LightArray a -> (Op -> (a -> a)) -> [String] -> IO ()
process lights ops ("turn" : "off" : coords) =
    runCoords lights (ops OpOff) coords
process lights ops ("turn" : "on" : coords) =
    runCoords lights (ops OpOn) coords
process lights ops ("toggle" : coords) =
    runCoords lights (ops OpToggle) coords
process _ _ _ = error "bad command"

solna :: String -> IO ()
solna stuff = do
  lights <- newArray ((0, 0), (bound, bound)) False :: IO (LightArray Bool)
  mapM_ (process lights opsEnglish) $ map words $ lines stuff
  t <- foldM (addLight lights) 0
             [(x, y) | x <- [0..bound], y <- [0..bound]]
  print (t :: Int)
  where
    addLight a n coord = do
      b <- readArray a coord
      if b then return (n + 1) else return n

solnb :: String -> IO ()
solnb stuff = do
  lights <- newArray ((0, 0), (bound, bound)) 0 :: IO (LightArray Int)
  mapM_ (process lights opsElvish) $ map words $ lines stuff
  t <- foldM (addLight lights) 0
             [(x, y) | x <- [0..bound], y <- [0..bound]]
  print (t :: Int)
  where
    addLight a n coord = do
      i <- readArray a coord
      return (n + i)

main :: IO ()
main = makeMain solna solnb
