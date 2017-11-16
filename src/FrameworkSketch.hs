{-#LANGUAGE GADTs, Rank2Types #-}

-- TODO: Tag-sets, filter/exclude by tag/tag-set, input type tags?, catch errors, common driver interface, tag-testing

type XBool = forall bool. Boolean bool => bool

data Property where
  Property :: Input a => (a -> XBool) -> Property
  PropertySet :: [Tag] -> [Property] -> Property

class Boolean a where
  fromBool  :: Bool -> a
  (.&&)     :: a -> a -> a
  (.||)     :: a -> a -> a
  nott      :: a -> a
  
  (-||)     :: a -> a -> a -- ^ Parallel OR
  (-||) = (.||)
  
  (-&&)     :: a -> a -> a -- ^ Parallel AND
  (-&&) = (.||)
  
  io :: IO a -> a
  io = error "IO actions are not supported by driver"
  
  -- ... + EVERY OTHER OPERATOR IN EXISTENCE!!!1

instance Boolean Bool where
  fromBool a = a
  (.&&) = (&&)
  (.||) = (||)
  nott = not

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
    = QCGen QCGenType
    | SizedFunctorGen
    | SCGen
    deriving (Show, Read, Eq)

data QCGenType = Manual | UniformFEAT | NicksStuff | Whatelse
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


-- Drivers are used to run properties
data Driver
    = FEAT FEATSettings
    | QuickCheck (QCSettings)
    | NEAT NEATSettings
    | SmallCheck SCSettings
    deriving (Show, Read, Eq)

data QCSettings = QCSettings {}
    deriving (Show, Read, Eq)

data FEATSettings = FEATSettings {}
    deriving (Show, Read, Eq)

data NEATSettings = NEATSettings {}
    deriving (Show, Read, Eq)

data SCSettings = SCSettings {}
    deriving (Show, Read, Eq)


-- Useable as property input. Should inherit from all generator-classes. 
class (MyGenerator a) => Input a
  -- where 
  --   -- | Optional input tags (things like #constructors)
  --   inputTags :: (a -> b) -> [InputTag]
  --   inputTags = const []

instance Input Bool
-- ... Input instances for all base classes.


drive = drive' ""

drive' :: String -> Driver -> Property -> IO (Int,Int)
drive' _ d (Property f) = myDriver f >>= return . maybe (0,1) (const (1,1)) -- TODO: choose real drivers
drive' indent d p@(PropertySet _ ps) = do
  let name = unwords (getName p)
  putStrLn $ unwords $ 
    [indent++"Entering property set"]++["("++name++")" | not $ null name]
  (solved,total) <- unzip `fmap` mapM (drive' (' ':' ':indent) d) ps
  let res = sum solved
      tot = sum total
  putStrLn $ unwords $
    [indent++"Exiting set,",show res,"of",show tot++" solved"]++["("++name++")" | not $ null name]
  return (res,tot)

getName :: Property -> [String]
getName (PropertySet ts ps) = [s|Named s <- ts]
getName _ = []






-- Example driver
main = drive undefined allprops >> return ()

class MyGenerator a where
  generate :: a

instance MyGenerator Bool where generate = False

myDriver :: MyGenerator a => (a -> Bool) -> IO (Maybe a)
myDriver p = return (if p generate then Nothing else Just generate)


-- Example problem set
allprops = PropertySet [Named "All properties"] 
  [ Property p_1
  , PropertySet [] [Property p_2]
  , PropertySet [Named "Lazy stuff", Lazy] [Property p_3a, Property p_3b]
  ]
  
p_1 :: Bool -> XBool
p_1 b = fromBool b

p_2 :: Bool -> XBool
p_2 b = fromBool (not b)

p_3a :: Bool -> XBool
p_3a _ = fromBool False

p_3b :: Bool -> XBool
p_3b _ = fromBool True


