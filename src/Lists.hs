{-# LANGUAGE FlexibleContexts #-}
module Lists where

import Property

prop_not_permutation_of :: Boolean bool => [Int] -> Property bool [Int]
prop_not_permutation_of xs = Prop $ not' . go xs
  where
    go [] []     = true 
    go xs (y:ys)
      | y `elem` xs = go (takeWhile (/= y) xs ++ drop 1 (dropWhile (/= y) xs)) ys
      | otherwise   = false 
    go _ _ = false 

prop_is_palindrome :: HasEq bool Int => Property bool [Int]
prop_is_palindrome = Prop $ \xs -> foldr (&&.) true (zipWith (==.) (reverse xs) xs)

prop_list_length :: HasOrd bool Int => Int -> Property bool [Int]
prop_list_length i = Prop $ \xs -> length xs <=. i

prop_long_palindrome :: (HasEq bool Int, HasOrd bool Int) => Int -> Property bool [Int]
prop_long_palindrome i = (Prop $ \xs -> length xs >= i) ==> inverse prop_is_palindrome
