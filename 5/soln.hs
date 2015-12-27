-- Copyright © 2015 Bart Massey

import Soln

-- | Check that all niceness properties hold.
nice :: String -> Bool
nice s =
    threeVowels && doubledLetter && noMagic
    where
      threeVowels = 3 <= length (filter (`elem` "aeiou") s)
      doubledLetter = any ((>= 2) . length) $ group s
      noMagic = not $ any (`elem` ["ab", "cd", "pq", "xy"]) $ tiles 2 1 s


solve :: (String -> Bool) -> String -> Int
solve f stuff = length $ filter f $ lines stuff

solna :: String -> IO ()
solna stuff = print $ solve nice stuff

-- | Check that all nicerness properties hold. The use of
-- 'shortenThrees' turns repeats of length exactly 3 into
-- repeats of length 2 to avoid false positives.
nicer :: String -> Bool
nicer s =
    matchingPairs && xyxs
    where
      matchingPairs =
          any ((>= 2) . length) $ group $ sort $ tiles 2 1 $
            concatMap shortenThrees $ group s
          where
            shortenThrees [x, _ , _] = [x, x]
            shortenThrees xs = xs
      xyxs =
          any xyx $ tiles 3 1 s
          where
            xyx [c1, _, c3] = c1 == c3
            xyx _ = error "bad tile length"
            
solnb :: String -> IO ()
solnb stuff = print $ solve nicer stuff

main :: IO ()
main = makeMain solna solnb
