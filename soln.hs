-- Copyright © 2015 Bart Massey

import Soln

solna :: String -> IO ()
solna stuff = do
  putStrLn stuff

solnb :: String -> IO ()
solnb stuff = do
  putStrLn stuff

main :: IO ()
main = makeMain solna solnb
