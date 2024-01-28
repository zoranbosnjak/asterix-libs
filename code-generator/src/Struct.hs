-- | Asterix structures and helper functions
--
-- Simplified structures for source code generation
-- Asterix database, which collects same items

{-
-- 'AsterixDb' - Asterix database is a collection of all components, without
-- duplications (the same definitions can be reused). For example,
-- 'I010/SAC' and 'I010/SIC' fields normally share exactly the same structure,
-- so it is stored in a database only once.
-}

module Struct where

import           Control.Monad.State
import           Data.List           (elemIndex)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)

import qualified Asterix.Specs       as A

-- | Item full name in reverse, like ["SAC", "010", "CAT_002"]
type Path = [A.Name]

-- | Size is bytes
type ByteSize = Int

-- | Offset inside octet [0..7]
newtype OctetOffset = OctetOffset Int
    deriving (Eq, Ord, Show)

instance Semigroup OctetOffset where
    OctetOffset x <> OctetOffset y = OctetOffset (mod (x+y) 8)

instance Monoid OctetOffset where
    mempty = OctetOffset 0

-- | Octet offset smart constructor
octetOffset :: Int -> OctetOffset
octetOffset n = OctetOffset (mod n 8)

data Content
    = ContentRaw
    | ContentTable [(Int, Text)]
    | ContentString A.StringType
    | ContentInteger A.Signedness
    | ContentQuantity A.Signedness A.Number A.Unit
    deriving (Eq, Ord, Show)

data Variation
    = Element OctetOffset A.RegisterSize Content
    | Group [Item]
    | Extended [Maybe Item]
    | Repetitive A.RepetitiveType Variation
    | Explicit (Maybe A.ExplicitType)
    | Compound (Maybe A.RegisterSize) [Maybe Item]
    deriving (Eq, Ord, Show)

data Item
    = Spare OctetOffset A.RegisterSize
    | Item A.Name A.Title Variation
    deriving (Eq, Ord, Show)

data Uap
    = Uap Variation
    | Uaps [(A.UapName, Variation)] (Maybe A.UapSelector)
    deriving (Eq, Ord, Show)

data AstSpec
    = AstCat Uap
    | AstRef Variation
    deriving (Eq, Show)

instance Ord AstSpec where
    compare (AstCat _) (AstRef _) = LT
    compare (AstRef _) (AstCat _) = GT
    compare (AstCat a) (AstCat b) = compare a b
    compare (AstRef a) (AstRef b) = compare a b

-- | Category number
type Cat = Int

-- | Derived Asterix structure
data Asterix = Asterix
    { astCat     :: Cat
    , astEdition :: A.Edition
    , astSpec    :: AstSpec
    } deriving (Eq, Show)

instance Ord Asterix where
    compare a b
        = compare (astCat a) (astCat b)
       <> compare (astEdition a) (astEdition b)
       <> compare (astSpec a) (astSpec b)

evalNumber :: Fractional a => A.Number -> a
evalNumber = \case
    A.NumInt i -> fromIntegral i
    A.NumDiv a b -> evalNumber a / evalNumber b
    A.NumPow a b -> fromIntegral (a ^ b)

deriveContent :: A.Content -> Content
deriveContent = \case
    A.ContentRaw -> ContentRaw
    A.ContentTable lst -> ContentTable lst
    A.ContentString st -> ContentString st
    A.ContentInteger sig _constr -> ContentInteger sig
    A.ContentQuantity sig lsb unit _constr -> ContentQuantity sig lsb unit
    A.ContentBds _t -> ContentRaw

deriveRule :: A.Rule -> Content
deriveRule = \case
    A.ContextFree cont -> deriveContent cont
    A.Dependent _ _ -> ContentRaw

-- | Split list by 'Nothing'
unconcatMaybe :: Eq a => [Maybe a] -> [[Maybe a]]
unconcatMaybe [] = []
unconcatMaybe xs = case succ <$> elemIndex Nothing xs of
    Nothing -> [xs]
    Just ix -> take ix xs : unconcatMaybe (drop ix xs)

-- | Assert assumption
assert :: Applicative f => String -> Bool -> f ()
assert _msg True = pure ()
assert msg False = error msg

-- | Assert byte alignment
byteAligned :: String -> State OctetOffset ()
byteAligned msg = do
    o <- get
    assert (msg <> ": bit offset " <> show o) (o == mempty)

deriveVariation :: A.Variation -> State OctetOffset Variation
deriveVariation = \case
    A.Element n rule -> do
        o <- get
        modify (<> octetOffset n)
        pure $ Element o n (deriveRule rule)
    A.Group lst -> do
        Group <$> mapM deriveItem lst
    A.Extended lst -> do
        items <- forM lst $ \case
            Nothing -> do
                modify (<> octetOffset 1) -- FX bit
                pure Nothing
            Just item -> Just <$> deriveItem item
        pure $ Extended items
    A.Repetitive rt var -> do
        byteAligned "repetitive (pre)"
        var2 <- deriveVariation var
        case rt of
            A.RepetitiveRegular _ -> pure ()
            A.RepetitiveFx -> modify (<> octetOffset 1) -- FX bit
        byteAligned "repetitive (post)"
        pure $ Repetitive rt var2
    A.Explicit t -> do
        byteAligned "explicit (pre)"
        pure $ Explicit t
    A.RandomFieldSequencing -> do
        error "Random Field Sequencing is not supported."
    A.Compound mn lst' -> do
        byteAligned "compound (pre)"
        items <- forM lst $ \case
            Nothing -> pure Nothing
            Just item -> Just <$> deriveItem item
        byteAligned "compound (post)"
        pure $ Compound mn items
      where
        removeRfs = \case
            Just (A.Item _name _title A.RandomFieldSequencing _doc) -> Nothing
            other -> other
        lst = fmap removeRfs lst'

deriveItem :: A.Item -> State OctetOffset Item
deriveItem = \case
    A.Spare n -> do
        o <- get
        modify (<> octetOffset n)
        pure $ Spare o n
    A.Item name title var _doc -> Item
        <$> pure name
        <*> pure title
        <*> deriveVariation var

deriveAsterix :: A.Asterix -> Asterix
deriveAsterix = \case
    A.AsterixBasic (A.Basic cat _title edition _date _preamble items uap) ->
        Asterix cat edition $ AstCat $ case uap of
            A.Uap lst -> Uap $ mkVar $ mkToplevel items lst
            A.Uaps lst2 sel ->
                let uaps = [(name, mkVar $ mkToplevel items lst) | (name, lst) <- lst2]
                in Uaps uaps sel
    A.AsterixExpansion (A.Expansion cat _title edition _date var) ->
        Asterix cat edition (AstRef $ mkVar var)
  where
    -- | Create toplevel compound item (which represents a category).
    mkToplevel :: [A.Item] -> [Maybe A.Name] -> A.Variation
    mkToplevel catalogue uap = A.Compound Nothing (fmap (fmap findItem) uap)
      where
        findItem :: A.Name -> A.Item
        findItem name = go catalogue
          where
            go [] = error "Item not found"
            go (A.Spare _n : xs) = go xs
            go (x@(A.Item iName _title _var _doc) : xs)
                | iName == name = x
                | otherwise = go xs

    mkVar :: A.Variation -> Variation
    mkVar var = evalState (deriveVariation var) mempty

-- | Database of all distinct asterix components. It's parametrized over some
-- container, to be used in different contexts.
data AsterixDb f = AsterixDb
    { dbContent   :: f Content
    , dbVariation :: f Variation
    , dbItem      :: f Item
    , dbUapSel    :: f A.UapSelector
    , dbUap       :: f Uap
    , dbSpec      :: f AstSpec
    , dbAst       :: f Asterix
    }

-- | Simple version of 'lens' over AsterixDb
data FocusDb f a = FocusDb
    { getDb :: AsterixDb f -> f a
    , setDb :: AsterixDb f -> f a -> AsterixDb f
    }

modifyDb :: FocusDb f a -> (f a -> f a) -> AsterixDb f -> AsterixDb f
modifyDb l f db = setDb l db $ f $ getDb l db

lContent :: FocusDb f Content
lContent = FocusDb dbContent (\db x -> db {dbContent = x})

lVariation :: FocusDb f Variation
lVariation = FocusDb dbVariation (\db x -> db {dbVariation = x})

lItem :: FocusDb f Item
lItem = FocusDb dbItem (\db x -> db {dbItem = x})

lUapSel :: FocusDb f A.UapSelector
lUapSel = FocusDb dbUapSel (\db x -> db {dbUapSel = x})

lUap :: FocusDb f Uap
lUap = FocusDb dbUap (\db x -> db {dbUap = x})

lSpec :: FocusDb f AstSpec
lSpec = FocusDb dbSpec (\db x -> db {dbSpec = x})

lAst :: FocusDb f Asterix
lAst = FocusDb dbAst (\db x -> db {dbAst = x})

dbInsert :: Ord a => FocusDb Set a -> a -> State (AsterixDb Set) ()
dbInsert l x = modify $ modifyDb l (Set.insert x)

saveContent :: Content -> State (AsterixDb Set) ()
saveContent = dbInsert lContent

saveVariation :: Variation -> State (AsterixDb Set) ()
saveVariation var = do
    dbInsert lVariation var
    case var of
        Element _o _n cont   -> saveContent cont
        Group lst         -> mapM_ saveItem lst
        Extended lst      -> mapM_ saveItem (catMaybes lst)
        Repetitive _t v   -> saveVariation v
        Explicit _t       -> pure ()
        Compound _t lst   -> mapM_ saveItem (catMaybes lst)

saveItem :: Item -> State (AsterixDb Set) ()
saveItem item = do
    dbInsert lItem item
    case item of
        Spare _o _n              -> pure ()
        Item _name _title var -> saveVariation var

saveUap :: Uap -> State (AsterixDb Set) ()
saveUap uap = do
    dbInsert lUap uap
    case uap of
        Uap var       -> saveVariation var
        Uaps lst msel -> do
            maybe (pure ()) (dbInsert lUapSel) msel
            forM_ lst $ \(_name, var) -> saveVariation var

saveAsterix :: Asterix -> State (AsterixDb Set) ()
saveAsterix ast@(Asterix _cat _ed spec) = do
    dbInsert lAst ast
    dbInsert lSpec spec
    case spec of
        AstCat uap -> saveUap uap
        AstRef var -> saveVariation var

-- | Create asterix database
asterixDb :: Foldable t => t Asterix -> AsterixDb Set
asterixDb lst = execState (mapM_ saveAsterix lst)
    (AsterixDb mempty mempty mempty mempty mempty mempty mempty)

-- | Enumerated distinct values.
newtype EMap a = EMap { unEMap :: Map a Int }

-- | Enumerated database items. Each Set element gets its sequence number.
enumDb :: AsterixDb Set -> AsterixDb EMap
enumDb db = AsterixDb
    { dbContent   = f dbContent
    , dbVariation = f dbVariation
    , dbItem      = f dbItem
    , dbUapSel    = f dbUapSel
    , dbUap       = f dbUap
    , dbSpec      = f dbSpec
    , dbAst       = f dbAst
    }
  where
    f :: Ord a => (AsterixDb Set -> Set a) -> EMap a
    f sel = EMap $ Map.fromList $ zip (Set.toAscList $ sel db) [0..]

-- | Get element's index.
indexOf :: Ord a => EMap a -> a -> Int
indexOf (EMap m) = (Map.!) m

enumList :: EMap k -> [(k, Int)]
enumList = Map.toList . unEMap
