-- Copyright © 2015 Bart Massey

import Soln

-- | Process a move command.
move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) c =
    case c of
      '^' -> (x, y + 1)
      'v' -> (x, y - 1)
      '>' -> (x + 1, y)
      '<' -> (x - 1, y)
      _ -> error "bad command"

-- | Get all the locations visited by a sequence of move
-- commands. The use of `mapAccumL` accumulates every
-- location visited in order.
getLocs :: String -> [(Int, Int)]
getLocs stuff =
    snd $ mapAccumL moveAndReport (0, 0) stuff
    where
      moveAndReport posn command =
          let posn' = move posn command in
          (posn', posn')

-- | Strategy: Find all visited locations, filter out
-- duplicates, and find the length of the resulting
-- sequence.
solna :: String -> IO ()
solna stuff = do
  print $ length $ nub $ getLocs stuff

-- | Split the sequence of moves up into a sequence
-- of odd-numbered moves and a sequence of even-numbered
-- moves. This construction is a bit forced, but the
-- obvious alternative requires explicit recursion.
alternates :: String -> (String, String)
alternates stuff =
  let (o, e) = partition (\(n, _) -> odd n) $
               zip [(1 :: Integer)..] stuff in
  (map snd o, map snd e)

-- | Strategy: Split out the moves for Santa and Robo-Santa,
-- accumulate both traces, and then merge and de-duplicate
-- them to get a list of all locations visited, and find the
-- length of that list.
solnb :: String -> IO ()
solnb stuff = do
  let (cmdsOdd, cmdsEven) = alternates stuff
  let locsOdd = getLocs cmdsOdd
  let locsEven = getLocs cmdsEven
  print $ length $ nub $ locsOdd ++ locsEven

main :: IO ()
main = makeMain solna solnb
