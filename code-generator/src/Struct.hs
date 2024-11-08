-- | Asterix structures and helper functions for source code generation
--

{-# LANGUAGE LambdaCase #-}

module Struct where

import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.State
import           Data.Bool
import           Data.Coerce
import           Data.Foldable       (toList)
import           Data.List           (elemIndex, unfoldr)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Word
import           GHC.Generics        (Generic)

import qualified Asterix.Specs       as A

-- | Fspec as list of octets
type Fspec = [Word8]

-- | Offset inside octet [0..7]
newtype OctetOffset = OctetOffset Int
    deriving (Generic, Eq, Ord, Show)

instance Semigroup OctetOffset where
    OctetOffset x <> OctetOffset y = OctetOffset (mod (x+y) 8)

instance Monoid OctetOffset where
    mempty = OctetOffset 0

-- | Octet offset smart constructor
octetOffset :: Int -> OctetOffset
octetOffset n = OctetOffset (mod n 8)

-- | Reuse structures from Asterix.Specs
type Variation = A.Variation OctetOffset
type NonSpare = A.NonSpare OctetOffset
type Item = A.Item OctetOffset
type UapItem = A.UapItem NonSpare

newtype Record = Record [UapItem]
    deriving (Generic, Eq, Ord, Show)

type Uap = A.Uap Record

data Expansion = Expansion (Maybe A.ByteSize) [Maybe NonSpare]
    deriving (Generic, Eq, Ord, Show)

data Asterix
    = AsterixBasic A.CatNum A.Edition Uap
    | AsterixExpansion A.CatNum A.Edition Expansion
    deriving (Generic, Eq, Show)

instance Ord Asterix where
    compare a b
        = compare (catOf a) (catOf b)
       <> compare (editionOf a) (editionOf b)
       <> cmp a b
      where
        catOf = \case
            AsterixBasic cat _ _ -> cat
            AsterixExpansion cat _ _ -> cat
        editionOf = \case
            AsterixBasic _ ed _ -> ed
            AsterixExpansion _ ed _ -> ed
        cmp (AsterixBasic {}) (AsterixExpansion {}) = LT
        cmp (AsterixExpansion {}) (AsterixBasic {}) = GT
        cmp (AsterixBasic _ _ uap1) (AsterixBasic _ _ uap2) = compare uap1 uap2
        cmp (AsterixExpansion _ _ e1) (AsterixExpansion _ _ e2) = compare e1 e2

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

-- | Derive Variation in State context.
deriveVariationM :: A.Variation o -> State OctetOffset Variation
deriveVariationM = \case
    A.Element _ (A.BitSize n) rule -> do
        o <- get
        modify (<> octetOffset n)
        pure $ A.Element o (A.BitSize n) rule
    A.Group _ lst -> do
        o <- get
        A.Group o <$> mapM deriveItemM lst
    A.Extended lst -> do
        items <- forM lst $ \case
            Nothing -> do
                modify (<> octetOffset 1) -- FX bit
                pure Nothing
            Just item -> Just <$> deriveItemM item
        pure $ A.Extended items
    A.Repetitive rt var -> do
        byteAligned "repetitive (pre)"
        var2 <- deriveVariationM var
        case rt of
            A.RepetitiveRegular _ -> pure ()
            A.RepetitiveFx        -> modify (<> octetOffset 1) -- FX bit
        byteAligned "repetitive (post)"
        pure $ A.Repetitive rt var2
    A.Explicit t -> do
        byteAligned "explicit (pre)"
        pure $ A.Explicit t
    A.Compound lst -> do
        byteAligned "compound (pre)"
        items <- forM lst $ \i -> do
            byteAligned "compound (pre subitem)"
            result <- case i of
                Nothing   -> pure Nothing
                Just item -> Just <$> deriveNonSpareM item
            byteAligned "compound (post subitem)"
            pure result
        byteAligned "compound (post)"
        pure $ A.Compound items

-- A.Item name title rule doc -> do
deriveNonSpareM  :: A.NonSpare o -> State OctetOffset NonSpare
deriveNonSpareM (A.NonSpare name title rule doc) = do
    -- We need to traverse the 'rule' and derive each variation inside
    -- the rule. But we also need to reset the offset back to the
    -- starting point on each step, such that the offset is eventually
    -- moved forward only once.
    o <- get -- this is starting offset
    let f x = put o >> deriveVariationM x -- reset offset on each step
    rule' <- traverse f rule
    pure $ A.NonSpare name title rule' doc

-- | Derive Item in State context.
deriveItemM :: A.Item o -> State OctetOffset Item
deriveItemM = \case
    A.Spare _ (A.BitSize n) -> do
        o <- get
        modify (<> octetOffset n)
        pure $ A.Spare o (A.BitSize n)
    A.Item nsp -> A.Item <$> deriveNonSpareM nsp

deriveNonSpare :: A.NonSpare o -> NonSpare
deriveNonSpare i = evalState (deriveNonSpareM i) mempty

deriveAsterix :: A.Asterix -> Asterix
deriveAsterix = \case
    A.AsterixBasic (A.Basic cat _title edition _date _preamb catalogue uap) ->
        AsterixBasic cat edition (deriveUap catalogue uap)
    A.AsterixExpansion (A.Expansion cat _title edition _date fspecSize items) ->
        AsterixExpansion cat edition (deriveExpansion fspecSize items)
  where
    deriveUap :: [A.NonSpare ()] -> A.Uap [A.UapItem A.ItemName] -> Uap
    deriveUap catalogue = fmap getRecord
      where
        items :: [NonSpare]
        items = fmap deriveNonSpare catalogue
        getItem :: A.ItemName -> NonSpare
        getItem name = head $ flip mapMaybe items $ \i -> case i of
            A.NonSpare iName _ _ _ -> bool Nothing (Just i) (iName == name)
        getRecord :: [A.UapItem A.ItemName] -> Record
        getRecord lst = Record $ fmap (fmap getItem) lst
    deriveExpansion :: Maybe A.ByteSize -> [Maybe (A.NonSpare ())] -> Expansion
    deriveExpansion mn items = Expansion mn (fmap (fmap deriveNonSpare) items)

-- | 'AsterixDb' - Asterix database is a collection of all components, without
-- duplications (the same definitions can be reused). For example,
-- items '010/SAC' and '010/SIC' normally share exactly the same structure,
-- so it is stored in a database only once.
-- Database is parametrized over some container, to be used in different contexts.
data AsterixDb f = AsterixDb
    { dbContent       :: f A.Content
    , dbRuleContent   :: f (A.Rule A.Content)
    , dbVariation     :: f Variation
    , dbRuleVariation :: f (A.Rule Variation)
    , dbNonSpare      :: f NonSpare
    , dbItem          :: f Item
    , dbUapItem       :: f UapItem
    , dbRecord        :: f Record
    , dbExpansion     :: f Expansion
    , dbUap           :: f Uap
    , dbAsterix       :: f Asterix
    }

-- | Enumerated distinct values.
newtype EMap a = EMap { unEMap :: Map a Int }

-- | Simple version of 'lens' over AsterixDb
data FocusDb f a = FocusDb
    { getDb :: AsterixDb f -> f a
    , setDb :: AsterixDb f -> f a -> AsterixDb f
    }

lContent :: FocusDb f A.Content
lContent = FocusDb dbContent (\db x -> db {dbContent = x})

lRuleContent :: FocusDb f (A.Rule A.Content)
lRuleContent = FocusDb dbRuleContent (\db x -> db {dbRuleContent = x})

lVariation :: FocusDb f Variation
lVariation = FocusDb dbVariation (\db x -> db {dbVariation = x})

lRuleVariation :: FocusDb f (A.Rule Variation)
lRuleVariation = FocusDb dbRuleVariation (\db x -> db {dbRuleVariation = x})

lNonSpare :: FocusDb f NonSpare
lNonSpare = FocusDb dbNonSpare (\db x -> db {dbNonSpare = x})

lItem :: FocusDb f Item
lItem = FocusDb dbItem (\db x -> db {dbItem = x})

lUapItem :: FocusDb f UapItem
lUapItem = FocusDb dbUapItem (\db x -> db {dbUapItem = x})

lRecord :: FocusDb f Record
lRecord = FocusDb dbRecord (\db x -> db {dbRecord = x})

lExpansion :: FocusDb f Expansion
lExpansion = FocusDb dbExpansion (\db x -> db {dbExpansion = x})

lUap :: FocusDb f Uap
lUap = FocusDb dbUap (\db x -> db {dbUap = x})

lAsterix :: FocusDb f Asterix
lAsterix = FocusDb dbAsterix (\db x -> db {dbAsterix = x})

modifyDb :: FocusDb f a -> (f a -> f a) -> AsterixDb f -> AsterixDb f
modifyDb l f db = setDb l db $ f $ getDb l db

dbInsert :: Ord a => FocusDb Set a -> a -> State (AsterixDb Set) ()
dbInsert l x = modify $ modifyDb l (Set.insert x)

dbDelete :: Ord a => FocusDb Set a -> a -> State (AsterixDb Set) ()
dbDelete l x = modify $ modifyDb l (Set.delete x)

saveContent :: A.Content -> State (AsterixDb Set) ()
saveContent = dbInsert lContent

saveRule :: Ord a
    => (a -> State (AsterixDb Set) ())
    -> FocusDb Set (A.Rule a)
    -> A.Rule a
    -> State (AsterixDb Set) ()
saveRule save focus rule = do
    dbInsert focus rule
    mapM_ save $ toList rule

saveVariation :: Variation -> State (AsterixDb Set) ()
saveVariation var = do
    dbInsert lVariation var
    case var of
        A.Element _o _n rule -> saveRule saveContent lRuleContent rule
        A.Group _o lst       -> mapM_ saveItem lst
        A.Extended lst       -> mapM_ saveItem (catMaybes lst)
        A.Repetitive _t v    -> saveVariation v
        A.Explicit _t        -> pure ()
        A.Compound lst       -> mapM_ saveNonSpare (catMaybes lst)

saveNonSpare :: NonSpare -> State (AsterixDb Set) ()
saveNonSpare nsp@(A.NonSpare _name _title rule _doc) = do
    dbInsert lNonSpare nsp
    saveRule saveVariation lRuleVariation rule

saveItem :: Item -> State (AsterixDb Set) ()
saveItem item = do
    dbInsert lItem item
    case item of
        A.Spare _o _n -> pure ()
        A.Item nsp    -> saveNonSpare nsp

saveUapItem :: UapItem -> State (AsterixDb Set) ()
saveUapItem uapItem = do
    dbInsert lUapItem uapItem
    case uapItem of
        A.UapItem nsp  -> saveNonSpare nsp
        A.UapItemSpare -> pure ()
        A.UapItemRFS   -> pure ()

saveRecord :: Record -> State (AsterixDb Set) ()
saveRecord x@(Record lst) = do
    dbInsert lRecord x
    mapM_ saveUapItem lst

saveExpansion :: Expansion -> State (AsterixDb Set) ()
saveExpansion x@(Expansion _n lst) = do
    dbInsert lExpansion x
    mapM_ (mapM_ saveNonSpare) lst

saveUap :: Uap -> State (AsterixDb Set) ()
saveUap uap = do
    dbInsert lUap uap
    mapM_ saveRecord uap

saveAsterix :: Asterix -> State (AsterixDb Set) ()
saveAsterix asterix = do
    dbInsert lAsterix asterix
    case asterix of
        AsterixBasic _cat _edition uap           -> saveUap uap
        AsterixExpansion _cat _edition expansion -> saveExpansion expansion

-- | Create asterix specs database
asterixDb :: Foldable t => t Asterix -> AsterixDb Set
asterixDb lst = execState (mapM_ saveAsterix lst)
    (AsterixDb mempty mempty mempty mempty mempty
     mempty mempty mempty mempty mempty mempty)

-- | Enumerated database items. Each Set element gets its sequence number.
enumDb :: AsterixDb Set -> AsterixDb EMap
enumDb db = AsterixDb
    (f dbContent)
    (f dbRuleContent)
    (f dbVariation)
    (f dbRuleVariation)
    (f dbNonSpare)
    (f dbItem)
    (f dbUapItem)
    (f dbRecord)
    (f dbExpansion)
    (f dbUap)
    (f dbAsterix)
  where
    f :: Ord a => (AsterixDb Set -> Set a) -> EMap a
    f sel = EMap $ Map.fromList $ zip (Set.toAscList $ sel db) [0..]

-- | Get element's index.
indexOf :: Ord a => EMap a -> a -> Int
indexOf (EMap m) = (Map.!) m

enumList :: EMap k -> [(k, Int)]
enumList = Map.toList . unEMap

-- | Split list of 'Maybe' values to the lists of equal size append 'Nothing'
chunksOf :: Int -> [Maybe a] -> [[Maybe a]]
chunksOf n = unfoldr f
  where
    f [] = Nothing
    f lst =
        let (a,b) = splitAt n lst
        in Just (take n (a <> repeat Nothing), b)

bitSizeOfVariation :: A.Variation OctetOffset -> Int
bitSizeOfVariation = \case
    A.Element _ n _ -> coerce n
    A.Group _ lst -> sum $ fmap bitSizeOfItem lst
    _ -> error "unexpected non-fixed argument"

bitSizeOfRule :: A.Rule (A.Variation OctetOffset) -> Int
bitSizeOfRule = \case
    A.ContextFree var -> bitSizeOfVariation var
    A.Dependent _ var _ -> bitSizeOfVariation var

bitSizeOfNonSpare :: A.NonSpare OctetOffset -> Int
bitSizeOfNonSpare (A.NonSpare _ _ rule _) = bitSizeOfRule rule

bitSizeOfItem :: Item -> Int
bitSizeOfItem = \case
    A.Spare _o n -> coerce n
    A.Item nsp -> bitSizeOfNonSpare nsp

fspecMaxBytes :: [a] -> Int
fspecMaxBytes lst = case divMod (length lst) 7 of    -- 1 bit for FX
    (n, 0) -> n
    (n, _) -> succ n

hasRFS :: Record -> Bool
hasRFS (Record items) = any isRFS items where
    isRFS = \case
        A.UapItemRFS -> True
        _ -> False
