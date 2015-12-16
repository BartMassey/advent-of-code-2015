-- Copyright © 2015 Bart Massey

import Soln

countEscaped :: String -> Int
countEscaped s =
    envelop countNext 0 s
    where
      countNext n "" = (n, "")
      countNext n ('\\' : '\\' : xs) = (n + 1, xs)
      countNext n ('\\' : '"' : xs) = (n + 1, xs)
      countNext n ('\\' : 'x' : d1 : d2 : xs)
          | isHexDigit d1 && isHexDigit d2 = (n + 1, xs)
          | otherwise = error "bad hex escape"
      countNext _ ('\\' : _) = error "bad backslash escape"
      countNext n (_ : xs) = (n + 1, xs)

countStringEscaped :: String -> Int
countStringEscaped s
    | head s == '"' && last s == '"' =
        countEscaped s - 2
    | otherwise = error "bad string"

solna :: String -> IO ()
solna stuff = do
  print $ sum $ map diff $ lines stuff
  where
    diff s = length s - countStringEscaped s

countEscapesString :: String -> Int
countEscapesString s =
    envelop countEscapes 2 s
    where
      countEscapes n "" = (n, "")
      countEscapes n ('"' : xs) = (n + 2, xs)
      countEscapes n ('\\' : xs) = (n + 2, xs)
      countEscapes n (_ : xs) = (n + 1, xs)
                      
solnb :: String -> IO ()
solnb stuff = do
  print $ sum $ map diff $ lines stuff
  where
    diff s = countEscapesString s - length s

main :: IO ()
main = makeMain solna solnb
