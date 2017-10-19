{-# LANGUAGE FlexibleContexts #-}
module Trees where

import Property

import Test.QuickCheck hiding (Property, (==>))

data Tree a = Leaf
            | Branch a (Tree a) (Tree a) deriving (Ord, Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where
      go 0 = return Leaf
      go d = oneof [ return Leaf, Branch <$> arbitrary <*> go (d - 1) <*> go (d - 1) ]

items :: Tree a -> [a]
items Leaf = []
items (Branch a l r) = a : items l ++ items r

height :: Tree a -> Int
height Leaf = 0
height (Branch _ l r) = 1 + max (height l) (height r)

prop_isBST :: (Boolean bool, HasOrd bool Int) => Property bool (Tree Int)
prop_isBST = Prop go
  where
    go Leaf = true 
    go (Branch a left right) =
      go left  &&. 
      go right &&.
      (foldl (&&.) true [ a >. l  | l <- items left  ]) &&.
      (foldl (&&.) true [ a <=. r | r <- items right ])

prop_height_greater_than :: (Boolean bool, HasOrd bool Int) => Int -> Property bool (Tree Int)
prop_height_greater_than i = Prop $ (>. i) . height

prop_not_isBST :: (Boolean bool, HasOrd bool Int) => Property bool (Tree Int)
prop_not_isBST = inverse prop_isBST

-- This is a really hard one for QuickCheck
prop_no_BSTs_larger_than :: (Boolean bool, HasOrd bool Int, HasOrd bool Int) => Int -> Property bool (Tree Int)
prop_no_BSTs_larger_than h = prop_height_greater_than h ==> prop_not_isBST
