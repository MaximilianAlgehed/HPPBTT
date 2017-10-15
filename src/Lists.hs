{-# LANGUAGE FlexibleContexts #-}
module Lists where

import Property

prop_not_permutation_of :: [Int] -> Property Bool [Int]
prop_not_permutation_of xs = Prop $ not . go xs
  where
    go [] []     = True
    go xs (y:ys)
      | y `elem` xs = go (takeWhile (/= y) xs ++ drop 1 (dropWhile (/= y) xs)) ys
      | otherwise   = False
    go _ _ = False

prop_is_palindrome :: Property Bool [Int]
prop_is_palindrome = Prop $ \xs -> reverse xs == xs

prop_not_palindrome :: Property Bool [Int]
prop_not_palindrome = Prop $ \xs -> reverse xs /= xs

prop_list_size :: HasOrd bool Int => Int -> Property bool [Int]
prop_list_size i = Prop $ \xs -> length xs <=. i
