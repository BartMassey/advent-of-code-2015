-- Copyright © 2015 Bart Massey

module Soln (
  tiles, envelop, splits, sspan,
  makeMain,
  module Control.Monad,
  module Control.Applicative,
  module Data.Bits,
  module Data.Char,
  module Data.Function,
  module Data.List,
  module Data.Maybe,
  module Data.Ord,
  module Data.Word )
where

import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Char
import Data.Function
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

-- | Given a list, return the list of all possible
-- ways to split that list into two parts.
splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)


-- | Like 'Data.List.span', but includes the matched element
-- on the left.
sspan :: (a -> Bool) -> [a] -> ([a], [a])
sspan _ [] = ([], [])
sspan f (x : xs) | f x = ([x], xs)
sspan f (x : xs) = let (l, r) = sspan f xs in (x : l, r)

-- | Package up all the input reading and argument parsing
-- into a single clean `main`.
makeMain :: (String -> IO ()) -> (String -> IO ()) -> IO ()
makeMain solna solnb = do
  args <- getArgs
  stuff <- readFile "input.txt"
  case length args of
    0 -> solna stuff
    1 -> solnb stuff
    _ -> error "too many args"
