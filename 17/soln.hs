-- Copyright © 2015 Bart Massey

import Soln

solna :: String -> IO ()

solns :: String -> [[Int]]
solns stuff =
    filter ((== 150) . sum) $ subsequences $ map read $ lines stuff

solna stuff = do
  print $ length $ solns stuff

countMinimal :: Ord b => (a -> b) -> [a] -> Int
countMinimal _ [] = 0
countMinimal f (e0 : es) =
    fst $ foldl' check (1, f e0) es
    where
      check (c, m) e =
          let m' = f e in
          case m' `compare` m of
            LT -> (1, m')
            EQ -> (c + 1, m)
            GT -> (c, m)

solnb :: String -> IO ()
solnb stuff = do
  print $ countMinimal length $ solns stuff

main :: IO ()
main = makeMain solna solnb
