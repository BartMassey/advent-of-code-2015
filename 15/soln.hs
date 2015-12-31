-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.


import Soln

-- | Make a list of all the ways that a sum
-- less than or equal to k can be achieved with n items,
-- including sums involving 0.
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

-- | Make a list of all the ways that a sum of
-- k can be achieved with n items, including
-- sums involving 0.
sumsTo :: Int -> Int -> [[Int]]
sumsTo 0 0 = [[]]
sumsTo k 0 | k > 0 = []
sumsTo k n | k >= 0 && n > 0 =
    map completeSum $ sumsUpTo k (n - 1)
    where
      completeSum partial =
          (k - sum partial) : partial
sumsTo _ _ = error "negative quantities"

-- | Given a comma-separated string description of an
-- ingredient, produce a list of attributes
parseIngredient :: [String] -> [Int]
parseIngredient (_ : properties) =
    map (parseProperty . fst) $ filter (odd . snd) $
      zip properties [(0 :: Int) ..]
    where
      parseProperty p | last p == ',' = parseProperty $ init p
      parseProperty p = read p
parseIngredient _ = error "bad ingredient"

-- | Parse all the ingredients.
parseIngredients :: String -> [[Int]]
parseIngredients stuff =
    map (parseIngredient . words) $ lines stuff

-- | Strategy: Try all possible combinations of
-- ingredients and pick the best-scoring.
soln :: ([[Int]] -> [Int] -> Maybe Int) -> String -> Int
soln scorer stuff =
    let ingredients = parseIngredients stuff
        scores =
            map (scorer ingredients) $ sumsTo 100 $ length ingredients
    in
    case maximum scores of
      Just bestScore -> bestScore
      Nothing -> error "no solution"

-- | Compute the dot product of two lists of integers.
dotProduct :: [Int] -> [Int] -> Int
dotProduct a b = sum $ zipWith (*) a b

-- | The score of a recipe is the product of the
-- non-negative ingredient sums. This collapses to 0
-- when any ingredient has a negative score. Calories
-- are assumed to be last and are ignored.
scoreRecipe :: [[Int]] -> [Int] -> Maybe Int
scoreRecipe ingredients r =
    Just $ product $ map ((`max` 0) . dotProduct r) $
      transpose $ map init ingredients

solna :: String -> IO ()
solna stuff = do
  print $ soln scoreRecipe stuff

-- | A special recipe is only scored if it has
-- exactly 500 calories.
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
