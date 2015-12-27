-- Copyright © 2015 Bart Massey

import Soln

-- | Returns the minimum two of three sides. The
-- subscripts are awkward here, but at least we
-- have the tuple lengths as part of the type for
-- static checking.
minSides :: (Int, Int, Int) -> (Int, Int)
minSides (n1, n2, n3) =
    let [m1, m2] = take 2 $ sort [n1, n2, n3] in
    (m1, m2)

-- | Convert a tuple description into a tuple.
parseTuple :: String -> (Int, Int, Int)
parseTuple s =
  case words $ map fixx s of
    [n1, n2, n3] -> (read n1, read n2, read n3)
    _ -> error "bad tuple"
  where
    fixx 'x' = ' '
    fixx c | isDigit c = c
    fixx _ = error "bad character in input"

-- | The cost of a minimum-area wrapping.
area :: (Int, Int, Int) -> Int
area sides@(n1, n2, n3) =
    let (m1, m2) = minSides sides in
    m1 * m2 + 2 * (n1 * n2 + n2 * n3 + n1 * n3)

-- | The volume of a box.
volume :: (Int, Int, Int) -> Int
volume (n1, n2, n3) = n1 * n2 * n3

-- | The ribbon footage of a box.
footage :: (Int, Int, Int) -> Int
footage sides =
    let (m1, m2) = minSides sides in
    2 * (m1 + m2) + volume sides

-- | Given a cost function for an individual box,
-- print the total cost of wrapping all boxes in the input.
soln :: ((Int, Int, Int) -> Int) -> String -> IO ()
soln fCost stuff =
  print $ sum $ map (fCost . parseTuple) $ lines stuff

solna :: String -> IO ()
solna = soln area

solnb :: String -> IO ()
solnb = soln footage

main :: IO ()
main = makeMain solna solnb
