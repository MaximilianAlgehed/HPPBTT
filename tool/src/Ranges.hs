{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Ranges where

import Property

prop_in_range :: forall bool. (Boolean bool, HasOrd bool Double) => (Double, Double) -> Property bool Double
prop_in_range (min, max) = Prop $ \x -> ifE (x <. min :: bool) false (x <=. max)

prop_not_in_range :: forall bool. (Boolean bool, HasOrd bool Double) => (Double, Double) -> Property bool Double 
prop_not_in_range (min, max) = Prop $ \x -> ifE (x <=. max :: bool) false (x <. min)
