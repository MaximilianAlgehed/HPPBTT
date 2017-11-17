{-#LANGUAGE Rank2Types#-}
-- | Imported by every property package. 
module Test.Properties.Runtime
 (module Test.Properties.Runtime
 ,module QC
 ,module E
 ,module SC
 )
 where

import qualified Test.QuickCheck as QC
import qualified Control.Enumerable as E
import qualified Test.SmallCheck as SC
import Test.QuickCheck(Arbitrary(..), elements)
import Control.Enumerable(Enumerable(..), datatype, c0,c1)

import System.Environment(getArgs)

-- TODO: Move to a separate file
type XBool = forall bool. Boolean bool => bool
class Boolean a where
    fromBool  :: Bool -> a
    (.&&)     :: a -> a -> a
    (.||)     :: a -> a -> a
    nott      :: a -> a
    
--    (~&&)     :: Bool -> Bool -> 
--    (~||)     :: Bool -> Bool -> 
    
    (==>)     :: Bool -> a -> a
    a ==> b = fromBool a .|| b 
    
    (-||)     :: a -> a -> a -- ^ Parallel OR
    (-||) = (.||)
  
    (-&&)     :: a -> a -> a -- ^ Parallel AND
    (-&&) = (.||)
  
    io :: IO a -> a
    io = error "IO actions are not supported by driver"
  
    -- ... + EVERY OTHER OPERATOR IN EXISTENCE!!!1
false, true :: XBool
false = fromBool False
true  = fromBool True

instance Boolean Bool where
    fromBool = id
    (.&&) = (&&)
    (.||) = (||)
    nott = not

instance Boolean QC.Property where
   fromBool = QC.property
   (==>) = (QC.==>)
   (.&&) = error "TBD1"
   (.||) = error "TBD2"
   nott  = error "TBD3"

-- Move to a separate file (?)
type XProp    = PropTree (Setup -> IO Res)
type PropData = PropTree [Int] -- ^ A path in the property-tree

data PropTree a
    = Property a     
    | PropertySet [Tag] [PropTree a]
    deriving (Show, Read, Eq)

propData :: XProp -> PropData
propData = go [] where
  go id (Property _)        = Property id
  go id (PropertySet ts ps) = 
    PropertySet ts [go (id++[n]) p|(n,p) <- zip [0..] ps]

lookupProp :: PropTree a -> [Int] -> a
lookupProp (Property x)       []     = x
lookupProp (PropertySet _ ps) (x:xs) = lookupProp (ps !! x) xs
lookupProp _                  _      = error "Invalid property ID"


-- Move to a separate file
-- | Tags for property sets
data Tag 
    = Generator GeneratorTag    -- ^ Suppports a data generation method
    | Named String              -- ^ This property has a name
    | Source SourceTag          -- ^ Origin of the bug/property
    | UseIf IfTag               -- ^ Uses conditionals
    | UseImplication ImpTag     -- ^ Uses preconditions/implications
    | UseQuantifiers            -- ^ Uses existential quantifiers or nested universal quantifiers
    | UseIO                     -- ^ Uses the IO-monad :)
    | LazyOperators             -- ^ Uses parallel boolean operators and such
    | UseType TypeTag           -- ^ Uses the specified type
    | BigType                   -- ^ Uses big types (20+ total constructors)
--    | Constructors Int          -- ^ The number of constructors in input types
    | IrregularType             -- ^ Uses an irregular type
    | SecondOrder               -- ^ Input contains first order functions
    | HigherOrder               -- ^ Input contains higher order functions
    | Lazy                      -- ^ Highly lazy in the input data
    | Eager                     -- ^ Always evaluates the input 
    | Difficult                 -- ^ Some drivers fail to find ctrex
    | ReferenceImplementation   -- ^ Based on comparing to a reference implementation
    deriving (Show, Read, Eq)

data GeneratorTag
    = QCGen QCGenTypeTag
    | SizedFunctorGen
    | SCGen
    deriving (Show, Read, Eq)

data QCGenTypeTag = Manual | UniformFEAT | NicksStuff | Whatelse
    deriving (Show, Read, Eq)

data IfTag = WeirdContinuousIf | NormalIf
    deriving (Show, Read, Eq)

data ImpTag = ToplevelImp | NestedImp
    deriving (Show, Read, Eq)

data TypeTag = Integer | Double | Strings
    deriving (Show, Read, Eq)

data SourceTag
    = ManualSource                  -- ^ Artificial bug manually introduced
    | Mutated                       -- ^ Artificial bug generated using code mutation, property written manually
    | Deployed                      -- ^ Bug found in deployed code
    deriving (Show, Read, Eq)




data Setup = Setup{driver :: Driver}
    deriving (Show, Read, Eq)

data Driver 
    = QC 
    | SC
    | FEAT
    | NEAT
    | Magic
    deriving (Show, Read, Eq)

type Time = Integer
data Res
    = Result Bool Time
    | Unsupported
    | Error String
    deriving (Show, Read, Eq)


-- | main function of each property package.
run :: XProp -> IO()
run ps = do
    xs <- getArgs
    case xs of 
      []         -> putStr $ show $ propData ps
      ["single"] -> do
        (s,i) <- fmap read getLine :: IO (Setup,[Int])
        putStr (show Unsupported) -- TODO: Do stuff
      _          -> error "Bad command"
    return ()

qc :: Arbitrary a => (a -> QC.Property) -> Setup -> IO Res
qc   _ _ = undefined
sc :: (a -> Bool) -> Setup -> IO Res
sc   _ _ = undefined
feat :: Enumerable a => (a -> XBool) -> Setup -> IO Res
feat _ _ = undefined
neat :: Enumerable a => (a -> XBool) -> Setup -> IO Res
neat _ _ = undefined





-- Framework for driver support
data Executable a = Executable
  { eQC    :: ((a -> XBool) -> Runner QC)
  , eNEAT  :: ((a -> XBool) -> Runner NEAT)
  , eFEAT  :: ((a -> XBool) -> Runner FEAT)
  , eSC    :: ((a -> XBool) -> Runner SC)
  }

-- Abstract type (could be a GADT if we where fancy)
data Runner d = Runner (Setup -> IO Res)
              | NoRunner
  -- R_QC | R_FEAT | R_NEAT | R_SC

baseEx :: Executable a
baseEx = Executable 
  (\_ -> NoRunner) 
  (\_ -> NoRunner)  
  (\_ -> NoRunner)  
  (\_ -> NoRunner) 

runQC :: Arbitrary a => (a -> XBool) -> Runner QC
runQC p = Runner (qc p)

runNEAT :: Enumerable a => (a -> XBool) -> Runner NEAT
runNEAT p = Runner (neat p)

runFEAT :: Enumerable a => (a -> XBool) -> Runner FEAT
runFEAT p = Runner (feat p)

runSC :: (a -> XBool) -> Runner SC
runSC p = Runner (sc p)

data QC
data NEAT
data FEAT
data SC

-- Used to adapt every property
prp :: Executable a -> (a -> XBool) -> XProp
prp e p = Property $ \s -> case driver s of 
    QC -> runs (eQC e p) s
    where 
      runs (Runner x) s = x s
      runs (NoRunner) s = return Unsupported


-- Just an example
pr :: (Arbitrary a, Enumerable a) => (a -> XBool) -> XProp
pr x = prp withQCandFNEAT x where 
  withQCandFNEAT = baseEx
    { eQC = runQC
    , eNEAT = runNEAT
    , eFEAT = runFEAT
    }


