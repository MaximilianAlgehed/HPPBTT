{-#LANGUAGE Rank2Types#-}
import Test.Properties.Runtime


-- FILE START
allProperties :: XProp
allProperties = 
  PropertySet [Named "Simple properties"] 
    [PropertySet [] 
      [ (p prop_listsaresmall)
      , (p prop_nomonkeys)
      ]
    , (p prop_isother)
    ]

-- Supports QC, NEAT and FEAT
p :: (Arbitrary a, Enumerable a) => (a -> XBool) -> XProp
p x = prp withQCandFNEAT x where 
  withQCandFNEAT = baseEx
    { eQC = runQC
    , eNEAT = runNEAT
    , eFEAT = runFEAT
    }

main = run allProperties

prop_listsaresmall :: [Int] -> XBool
prop_listsaresmall xs = fromBool (sum xs < 100)

prop_nomonkeys "monkey!" = false
prop_nomonkeys _         = true -- Probably no monkeys


data MyType = MyConstructor | MyOtherConstructor

prop_isother MyOtherConstructor = true
prop_isother _                  = false

instance Arbitrary MyType where
  arbitrary = elements [MyConstructor, MyOtherConstructor]

instance Enumerable MyType where
  enumerate = datatype [c0 MyConstructor, c0 MyOtherConstructor]