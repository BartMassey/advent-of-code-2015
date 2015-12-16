-- Copyright © 2015 Bart Massey

module Soln (
  tiles, envelop,
  makeMain,
  module Control.Monad,
  module Control.Applicative,
  module Data.Bits,
  module Data.Char,
  module Data.List,
  module Data.Maybe,
  module Data.Ord,
  module Data.Word )
where

import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word

import System.Environment (getArgs)

-- | Left fold of accumulating function onto a list.
-- Runs until the accumulating function has consumed
-- the list, then returns the accumulator. Generalizes
-- 'Data.List.mapAccumL'.
envelop :: (b -> [a] -> (b, [a])) -> b -> [a] -> b
envelop _ a [] = a
envelop f a xs = uncurry (envelop f) (f a xs)

-- | Given a tile size and a skip count, break
-- a list into tiles. The tiles may overlap or
-- be disjoint. Partial tiles are discarded.
tiles :: Int -> Int -> [a] -> [[a]]
tiles size skip xs =
    envelop tile [] xs
    where
      tile partial rest 
          | length rest < size = (partial, [])
          | otherwise = (partial ++ [take size rest], drop skip rest)

makeMain :: (String -> IO ()) -> (String -> IO ()) -> IO ()
makeMain solna solnb = do
  args <- getArgs
  stuff <- readFile "input.txt"
  case length args of
    0 -> solna stuff
    1 -> solnb stuff
    _ -> error "too many args"
