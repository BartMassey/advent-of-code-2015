-- Copyright © 2015 Bart Massey

import Soln

-- | PRNG parameters.
prngA, prngM, prngSeed :: Word64
prngA = 252533
prngM = 33554393
prngSeed = 20151125

-- | Multiplication mod 'prngM'
modTimes :: Word64 -> Word64 -> Word64
modTimes x y = (x * y) `mod` prngM

-- | Squaring mod 'prngM'
modSquare :: Word64 -> Word64
modSquare x = x `modTimes` x

-- | 'prngA' to the given power, mod 'prngM'
modExpA :: Int -> Word64
modExpA e | e < 0 = error "negative exponent"
modExpA 0 = 1
modExpA 1 = prngA
modExpA e | even e = modSquare $ modExpA $ e `div` 2
modExpA e = prngA `modTimes` modExpA (e - 1)

-- | Output of PRNG after given number of rounds.
prngRounds :: Int -> Word64
prngRounds rounds =
    prngSeed `modTimes` modExpA rounds

-- | Calculate the index position of a given row and column.
index :: (Int, Int) -> Int
index (r, c) = (r + c) * (r + c + 1) `div` 2 + c

-- | Dig the row and column positions out of the input.
parsePosition :: String -> (Int, Int)
parsePosition stuff =
    case separate numbers of
      [rs, cs] -> (read rs, read cs)
      _ -> error "mysterious input"
    where
      numbers = filter okChar stuff
      okChar c = isDigit c || c == ','
      separate s =
          words $ map commaSpace s
          where
            commaSpace ',' = ' '
            commaSpace c = c

main :: IO ()
main = do
  stuff <- readFile "input.txt"
  let (r, c) = parsePosition stuff
  print $ prngRounds $ index (r - 1, c - 1)
