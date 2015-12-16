-- Copyright © 2015 Bart Massey

import Soln

solna :: String -> IO ()
solna stuff = do
  print (foldl' traverse 0 stuff :: Int)
  where
    traverse n '(' = n + 1
    traverse n ')' = n - 1
    traverse n _ = n

solnb :: String -> IO ()
solnb stuff = do
  case foldl' traverse (Right (1 :: Int, 0 :: Int)) stuff of
    Left n -> print n
    Right _ -> error "basement never entered"
  where
    traverse (Right (n, c)) '(' = Right (n + 1, c + 1)
    traverse (Right (n, c)) ')' | c == 0 = Left n
    traverse (Right (n, c)) ')' = Right (n + 1, c - 1)
    traverse enc _ = enc

main :: IO ()
main = makeMain solna solnb
