-- Copyright © 2015 Bart Massey

import Soln

traverse :: (Int, Int) -> Char -> (Int, Int)
traverse (x, y) '^' = (x, y + 1)
traverse (x, y) 'v' = (x, y - 1)
traverse (x, y) '>' = (x + 1, y)
traverse (x, y) '<' = (x - 1, y)
traverse xy _ = xy

double :: a -> (a, a)
double t = (t, t)

getLocs :: String -> [(Int, Int)]
getLocs stuff =
    snd $ mapAccumL (\t c -> double $ traverse t c) (0, 0) stuff

solna :: String -> IO ()
solna stuff = do
  print $ length $ nub $ sort $ getLocs stuff

alternates :: String -> (String, String)
alternates stuff =
  let (o, e) = partition (\(n, _) -> odd n) $
               zip [(1 :: Integer)..] stuff in
  (map snd o, map snd e)

solnb :: String -> IO ()
solnb stuff = do
  let (cmdsOdd, cmdsEven) = alternates stuff
  let locsOdd = getLocs cmdsOdd
  let locsEven = getLocs cmdsEven
  print $ length $ nub $ sort $ locsOdd ++ locsEven

main :: IO ()
main = makeMain solna solnb
