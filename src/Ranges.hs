module Ranges where

import Property

prop_in_range :: (IsBool bool, HasOrd bool Double) => (Double, Double) -> Property bool Double
prop_in_range (min, max) = Prop $ \x -> ifE (x <. min) false (x <=. max)

prop_not_in_range :: (IsBool bool, HasOrd bool Double) => (Double, Double) -> Property Bool Double 
prop_not_in_range (min, max) = Prop $ \x -> ifE (x <=. max) false (x <. min)
