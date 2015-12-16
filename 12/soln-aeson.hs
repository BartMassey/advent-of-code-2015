{-# LANGUAGE OverloadedStrings #-}
-- Copyright © 2015 Bart Massey

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

import Soln

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
