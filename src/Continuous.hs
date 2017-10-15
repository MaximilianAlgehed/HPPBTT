{-# LANGUAGE FlexibleContexts #-}
module Continuous where

import Property

make_prop_is_positive :: (IsBool bool, HasOrd bool Double) => ((Double, Double) -> Double) -> Property bool (Double, Double)
make_prop_is_positive f = Prop $ \x -> f x >. 0

make_prop_shifted_by :: Property bool (Double, Double) -> (Double, Double) -> Property bool (Double, Double)
make_prop_shifted_by prop (x', y') = let Prop p = prop in Prop $ \(x, y) -> p (x + x', y + y')

-- Ackley function
-- Shifted down by 1 to give it more values below zero
-- http://www.sfu.ca/~ssurjano/ackley.html
prop_ackley :: (IsBool bool, HasOrd bool Double) => Property bool (Double, Double)
prop_ackley = make_prop_is_positive ackley
  where
    ackley (x, y) = 19 - 20 * exp (-0.2 * sqrt ((x^2 + y^2) / 2)) - exp ((cos (2*pi*x) + cos (2*pi*y)) / 2) + exp 1

prop_ackley_shifted_by :: (IsBool bool, HasOrd bool Double) => (Double, Double) -> Property bool (Double, Double)
prop_ackley_shifted_by = make_prop_shifted_by prop_ackley

-- Dropwave function
-- Shifted up by 0.5 to give it fewer values below zero
-- http://www.sfu.ca/~ssurjano/drop.html
prop_drop_wave :: (IsBool bool, HasOrd bool Double) => Property bool (Double, Double)
prop_drop_wave = make_prop_is_positive dropwave
  where
    dropwave (x, y) = 0.5 - (1 + cos (12 * sqrt (x^2 + y^2))) / (0.5 * (x^2 + y^2) + 2)

prop_drop_wave_shifted_by :: (IsBool bool, HasOrd bool Double) => (Double, Double) -> Property bool (Double, Double)
prop_drop_wave_shifted_by = make_prop_shifted_by prop_drop_wave 

-- Rastrigin function
-- Shifted down by 0.5 to give it more values below zero
-- http://www.sfu.ca/~ssurjano/rastr.html
prop_rastrigin :: (IsBool bool, HasOrd bool Double) => Property bool (Double, Double)
prop_rastrigin = make_prop_is_positive rastrigin
  where
    rastrigin (x, y) = 19.5 + (x^2 - 10 * cos (2 * pi * x) + y^2 - 10 * cos (2 * pi * y))

prop_rastrigin_shifted_by :: (IsBool bool, HasOrd bool Double) => (Double, Double) -> Property bool (Double, Double)
prop_rastrigin_shifted_by = make_prop_shifted_by prop_rastrigin
