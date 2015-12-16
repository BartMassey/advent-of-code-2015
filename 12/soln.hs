-- Copyright © 2015 Bart Massey

import qualified Data.Map.Strict as M
import Numeric
import Text.JSON.Yocto

import Soln

soln :: Bool -> String -> IO ()
soln ignoreRed stuff = do
  print $ (fromRat $ addJson $ decode stuff :: Double)
  where
    addJson (Number n) = n
    addJson (Array vs) = sum $ map addJson vs
    addJson (Object m) | ignoreRed && String "red" `elem` M.elems m = 0
    addJson (Object m) = sum $ map addJson $ M.elems m
    addJson _ = 0

solna :: String -> IO ()
solna stuff = soln False stuff

solnb :: String -> IO ()
solnb stuff = soln True stuff

main :: IO ()
main = makeMain solna solnb
