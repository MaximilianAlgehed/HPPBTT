{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
  #-}
module Property where

data Property bool a = Prop (a -> bool)

testWith :: ((a -> bool) -> IO ()) -> Property bool a -> IO ()
testWith tool (Prop p) = tool p

apply :: Property bool a -> a -> bool 
apply (Prop p) a = p a

class IsBool bool where
  true, false :: bool
  ifE         :: bool -> bool -> bool -> bool

class HasOrd bool t where
  (<.)  :: t -> t -> bool
  (<=.) :: t -> t -> bool

instance Ord t => HasOrd Bool t where
  (<.) = (<)
  (<=.) = (<=)

not' :: IsBool bool => bool -> bool
not' x = ifE x false true

(>.) :: (IsBool bool, HasOrd bool t) => t -> t -> bool
x >. y = not' (x <=. y)
