{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ConstraintKinds
  #-}
module Property where

data Property bool a = Prop (a -> bool)

testWith :: ((a -> bool) -> IO ()) -> Property bool a -> IO ()
testWith tool (Prop p) = tool p

apply :: Property bool a -> a -> bool 
apply (Prop p) a = p a

inverse :: Boolean bool => Property bool a -> Property bool a
inverse (Prop p) = Prop (not' . p)

andAlso :: Boolean bool => Property bool a -> Property bool a -> Property bool a
andAlso (Prop p) (Prop q) = Prop $ \x -> p x &&. q x

type Boolean bool = (IsBool bool, HasIf bool bool)

class IsBool bool where
  true, false :: bool

instance IsBool Bool where
  true  = True
  false = False

class HasIf bool t where
  ifE :: bool -> t -> t -> t

instance HasIf Bool t where
  ifE a b c = if a then b else c

class HasOrd bool t where
  (<.)  :: t -> t -> bool
  (<=.) :: t -> t -> bool

instance Ord t => HasOrd Bool t where
  (<.)  = (<)
  (<=.) = (<=)

(&&.) :: Boolean bool => bool -> bool -> bool
a &&. b = ifE a b false

not' :: Boolean bool => bool -> bool
not' x = ifE x false true

(>.) :: (Boolean bool, HasOrd bool t) => t -> t -> bool
x >. y = not' (x <=. y)
