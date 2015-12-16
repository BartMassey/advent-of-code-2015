-- Copyright © 2015 Bart Massey

import Soln

minSides :: (Int, Int, Int) -> (Int, Int)
minSides (n1, n2, n3) =
    let [m1, m2] = take 2 $ sort [n1, n2, n3] in
    (m1, m2)


tuples :: String -> (Int, Int, Int)
tuples s =
  case words $ map fixx s of
    [n1, n2, n3] -> (read n1, read n2, read n3)
    _ -> error "bad tuple"
  where
    fixx 'X' = ' '
    fixx 'x' = ' '
    fixx c = c

area :: (Int, Int, Int) -> Int
area sides@(n1, n2, n3) =
    let (m1, m2) = minSides sides in
    m1 * m2 + 2 * (n1 * n2 + n2 * n3 + n1 * n3)

volume :: (Int, Int, Int) -> Int
volume (n1, n2, n3) = n1 * n2 * n3

footage :: (Int, Int, Int) -> Int
footage sides =
    let (m1, m2) = minSides sides in
    2 * (m1 + m2) + volume sides

soln :: ((Int, Int, Int) -> Int) -> String -> IO ()
soln f stuff =
  print $ sum $ map (f . tuples) $ lines stuff

solna :: String -> IO ()
solna = soln area

solnb :: String -> IO ()
solnb = soln footage

main :: IO ()
main = makeMain solna solnb
