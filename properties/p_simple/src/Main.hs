{-#LANGUAGE Rank2Types#-}
import Test.Properties.Runtime


-- FILE START
allProperties :: XProp
allProperties = 
  PropertySet [Named "Simple properties"] 
    [PropertySet [] 
      [ Property (p prop_listsaresmall)
      , Property (p prop_nomonkeys)
      ]
    , Property (p prop_isother)
    ]

p :: (Arbitrary a, Enumerable a) => 
  (a -> XBool) -> Setup -> IO Res
p p d = case driver d of
  QC{}   -> qc p d 
  SC{}   -> sc p d 
  FEAT{} -> feat p d
  NEAT{} -> neat p d
  _      -> return Unsupported

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