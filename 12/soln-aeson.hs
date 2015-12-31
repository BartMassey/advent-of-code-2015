-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

import Soln

-- | Print the sum of all the numbers except optionally
-- not the numbers in red objects.
soln :: Bool -> String -> IO ()
soln ignoreRed stuff = do
  print $ addJson $ fromJust $ decode $ B.pack stuff
  where
    addJson (Number n) = n
    addJson (Array a) = sum $ map addJson $ V.toList a
    addJson (Object m) | ignoreRed && String "red" `elem` M.elems m = 0
    addJson (Object m) = sum $ map addJson $ M.elems m
    addJson _ = 0

solna :: String -> IO ()
solna stuff = soln False stuff

solnb :: String -> IO ()
solnb stuff = soln True stuff

main :: IO ()
main = makeMain solna solnb
