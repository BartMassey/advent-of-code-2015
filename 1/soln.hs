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
  case foldl' traverse (Right (1, 0)) stuff of
    Left n -> print n
    Right _ -> error "basement never entered"
  where
    traverse :: State -> Char -> State
    traverse (Right (count, floor)) '(' =
        Right (count + 1, floor + 1)
    traverse (Right (count, floor)) ')'
        -- Going down from floor 0 enters the basement.
        | floor == 0 = Left count
        | otherwise = Right (count + 1, floor - 1)
    traverse count@(Left _) _ = count
    traverse _ _ = error "bad input or state"

main :: IO ()
main = makeMain solna solnb
