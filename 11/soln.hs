-- Copyright © 2015 Bart Massey

import Soln

ascending :: String -> Bool
ascending [c1, c2, c3] =
    ord c1 + 1 == ord c2 &&
    ord c2 + 1 == ord c3
ascending _ = error "bad tile length"

confused :: String -> Bool
confused s = any (`elem` s) ['i', 'o', 'l']

pairs :: String -> Int
pairs s =
    sum $ map ((`div` 2) . length) (group s)

incr :: String -> String
incr s =
    fst $ foldr incrChar ("", 1) s
    where
      incrChar c (cs, carry) =
          let c' = ord(c) + carry - ord('a') in
          (chr ((c' `mod` 26) + ord 'a') : cs,
           0 `max` (c' - 25))

nextPw :: String -> String
nextPw s0 = fromJust $ find valid $ tail $ iterate incr s0
  where
    valid s = any ascending (tiles 3 1 s) &&
              not (confused s) &&
              pairs s >= 2

solna :: String -> IO ()
solna stuff = do
  putStrLn $ nextPw stuff

solnb :: String -> IO ()
solnb stuff = do
  putStrLn $ nextPw $ nextPw stuff

main :: IO ()
main = makeMain solna solnb
