-- Copyright © 2015 Bart Massey

import Numeric
import Text.JSON

import Soln

soln :: Bool -> String -> IO ()
soln ignoreRed stuff = do
  print $ (fromRat $ addJson $ fromOk $ decode stuff :: Double)
  where
    fromOk (Ok v) = v
    fromOk _ = error "not ok"
    addJson (JSRational _ n) = n
    addJson (JSArray vs) = sum $ map addJson vs
    addJson (JSObject m)
        | ignoreRed && hasRed = 0
        | otherwise = sum $ map addJson values
        where
          values = map snd (fromJSObject m)
          hasRed = JSString (toJSString "red") `elem` values
    addJson _ = 0

solna :: String -> IO ()
solna stuff = soln False stuff

solnb :: String -> IO ()
solnb stuff = soln True stuff

main :: IO ()
main = makeMain solna solnb
