-- Copyright © 2015 Bart Massey

import Soln

sumsUpTo :: Int -> Int -> [[Int]]
sumsUpTo k n =
    iterate extendSums [[]] !! n
    where
      extendSums :: [[Int]] -> [[Int]]
      extendSums partials =
          concatMap extendSum partials
          where
            extendSum partial =
                map (: partial) [0 .. k - sum partial]

sumsTo :: Int -> Int -> [[Int]]
sumsTo 0 0 = [[]]
sumsTo k 0 | k > 0 = []
sumsTo k n | k >= 0 && n > 0 =
    map completeSum $ sumsUpTo k (n - 1)
    where
      completeSum partial =
          (k - sum partial) : partial
sumsTo _ _ = error "negative quantities"

parseIngredient :: [String] -> [Int]
parseIngredient (_ : properties) =
    map (parseProperty . fst) $ filter (odd . snd) $
      zip properties [(0 :: Int) ..]
    where
      parseProperty p | last p == ',' = parseProperty $ init p
      parseProperty p = read p
parseIngredient _ = error "bad ingredient"

parseIngredients :: String -> [[Int]]
parseIngredients stuff =
    map (parseIngredient . words) $ lines stuff

soln :: ([[Int]] -> [Int] -> Maybe Int) -> String -> Int
soln scorer stuff =
    fromJust $ maximum $ map (scorer ingredients) $
       sumsTo 100 $ length ingredients
    where
      ingredients = parseIngredients stuff

dotProduct :: [Int] -> [Int] -> Int
dotProduct a b = sum $ zipWith (*) a b

scoreRecipe :: [[Int]] -> [Int] -> Maybe Int
scoreRecipe ingredients r =
    Just $ product $ map ((`max` 0) . dotProduct r) $
      transpose $ map init ingredients

solna :: String -> IO ()
solna stuff = do
  print $ soln scoreRecipe stuff

scoreSpecialRecipe :: [[Int]] -> [Int] -> Maybe Int
scoreSpecialRecipe ingredients r
    | totalCalories == 500 = scoreRecipe ingredients r
    | otherwise = Nothing
    where
      totalCalories =
          dotProduct r $ map last ingredients

solnb :: String -> IO ()
solnb stuff = do
  print $ soln scoreSpecialRecipe stuff

main :: IO ()
main = makeMain solna solnb
