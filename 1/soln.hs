-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Prelude hiding (traverse, floor)
import Soln

-- | Strategy: Just traverse the floors.
solna :: String -> IO ()
solna stuff = do
  print $ foldl' traverse 0 stuff
  where
    traverse :: Int -> Char -> Int
    traverse floor '(' = floor + 1
    traverse floor ')' = floor - 1
    traverse _ _ = error "bad direction"

-- | 'Left' is number of steps needed to hit the basement.
-- | 'Right' is number of steps so far, and current floor number.
type State = Either Int (Int, Int)

-- | Strategy: Traverse the floors keeping count.
solnb :: String -> IO ()
solnb stuff = do
  case foldM traverse (1, 0) stuff of
    Left n -> print n
    Right _ -> error "basement never entered"
  where
    traverse :: (Int, Int) -> Char -> State
    traverse (count, floor) dirn =
        case dirn of
          '(' -> Right (count + 1, floor + 1)
          -- Going down from floor 0 enters the basement.
          ')' | floor == 0 -> Left count
          ')' -> Right (count + 1, floor - 1)
          _ -> error "bad direction"

main :: IO ()
main = makeMain solna solnb
