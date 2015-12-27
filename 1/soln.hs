-- Copyright © 2015 Bart Massey

import Soln

-- | Strategy: Just traverse the floors.
solna :: String -> IO ()
solna stuff = do
  print $ foldl' traverse 0 stuff
  where
    traverse :: Int -> Char -> Int
    traverse floor '(' = floor + 1
    traverse floor ')' = floor - 1
    traverse floor _ = error "bad direction"

-- | Strategy: Traverse the floors keeping count.
solnb :: String -> IO ()
solnb stuff = do
  case foldl' traverse (Right (1, 0)) stuff of
    Left n -> print n
    Right _ -> error "basement never entered"
  where
    traverse :: Either Int (Int, Int)
    traverse (Right (count, floor)) '(' =
        Right (count + 1, floor + 1)
    traverse (Right (count, floor)) ')'
        -- Going down from floor 0 enters the basement.
        | floor == 0 = Left count
        | otherwise = Right (count + 1, floor - 1)
    traverse _ _ = error "bad floor"

main :: IO ()
main = makeMain solna solnb
