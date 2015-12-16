-- Copyright © 2015 Bart Massey

import Crypto.Hash.MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Soln

byteHex :: Word8 -> String
byteHex b = map intToDigit [ fromIntegral b `shiftR` 4,
                             fromIntegral b .&. 0xf ]

showHex :: [Word8] -> String
showHex bs = concatMap byteHex bs

hashString :: String -> String
hashString s =
  showHex $ B.unpack $ hash $ BC.pack s

solve :: String -> Int -> Int
solve stuff digits =
    fromJust $ find valid [1..]
    where
      valid n = (replicate digits '0') ==
                (take digits $ hashString $ stuff ++ show n)

solna :: String -> IO ()
solna stuff = do
  print $ solve stuff 5

solnb :: String -> IO ()
solnb stuff = do
  print $ solve stuff 6

main :: IO ()
main = makeMain solna solnb
