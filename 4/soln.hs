-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Crypto.Hash.MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Parallel.Strategies

import Soln

-- | Return the hex representation of a byte. There
-- are other ways, but this is fast and easy to understand.
byteHex :: Word8 -> String
byteHex b =
    map intToDigit [ fromIntegral b `shiftR` 4,
                     fromIntegral b .&. 0xf ]

-- | Return the hex representation of a list of bytes.
showHex :: [Word8] -> String
showHex bs = concatMap byteHex bs

-- | Return the hex representation of the
-- MD5 hash of the given input string.
hashString :: String -> String
hashString s =
  showHex $ B.unpack $ hash $ BC.pack s

-- | Strategy: Brute force.
solve :: String -> Int -> Int
solve stuff digits =
    head $ catMaybes $ evalBuffer 256 rseq `withStrategy` map valid [1..]
    where
      valid n
          | replicate digits '0' ==
            (take digits $ hashString $ stuff ++ show n) =
                Just n
          | otherwise = Nothing

solna :: String -> IO ()
solna stuff = do
  print $ solve stuff 5

solnb :: String -> IO ()
solnb stuff = do
  print $ solve stuff 6

main :: IO ()
main = makeMain solna solnb
