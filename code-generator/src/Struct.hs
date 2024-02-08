-- | Asterix structures and helper functions for source code generation
--

{-
-- 'AsterixDb' - Asterix database is a collection of all components, without
-- duplications (the same definitions can be reused). For example,
-- 'I010/SAC' and 'I010/SIC' fields normally share exactly the same structure,
-- so it is stored in a database only once.
-}

module Struct where

import           Control.Applicative
import           Control.Monad.RWS
import           Control.Monad.State
import           Data.Bool
import           Data.List           (elemIndex, unfoldr)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Word
import           GHC.Generics        (Generic)

import qualified Asterix.Specs       as A

-- | Item full name in reverse, like ["SAC", "010", "CAT_002"]
type Path = [A.Name]

-- | Size is bytes
type ByteSize = Int

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

data Variation
    = Element OctetOffset A.RegisterSize A.Rule
    | Group [Item]
    | Extended [Maybe Item]
    | Repetitive A.RepetitiveType Variation
    | Explicit (Maybe A.ExplicitType)
    | RandomFieldSequencing
    | Compound (Maybe A.RegisterSize) [Maybe Item]
    deriving (Generic, Eq, Ord, Show)

data Item
    = Spare OctetOffset A.RegisterSize
    | Item A.Name A.Title Variation
    deriving (Generic, Eq, Ord, Show)

data Uap
    = Uap Variation
    | Uaps [(A.UapName, Variation)] (Maybe A.UapSelector)
    deriving (Generic, Eq, Ord, Show)

data AstSpec
    = AstCat Uap
    | AstRef Variation
    deriving (Generic, Eq, Show)

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
    } deriving (Generic, Eq, Show)

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
        pure $ Element o n rule
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
            A.RepetitiveFx        -> modify (<> octetOffset 1) -- FX bit
        byteAligned "repetitive (post)"
        pure $ Repetitive rt var2
    A.Explicit t -> do
        byteAligned "explicit (pre)"
        pure $ Explicit t
    A.RandomFieldSequencing -> do
        pure RandomFieldSequencing
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
    { dbContent   :: f A.Content
    , dbRule      :: f A.Rule
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

lContent :: FocusDb f A.Content
lContent = FocusDb dbContent (\db x -> db {dbContent = x})

lRule :: FocusDb f A.Rule
lRule = FocusDb dbRule (\db x -> db {dbRule = x})

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

saveContent :: A.Content -> State (AsterixDb Set) ()
saveContent = dbInsert lContent

saveRule :: A.Rule -> State (AsterixDb Set) ()
saveRule rule = do
    case rule of
        A.ContextFree cont    -> saveContent cont
        A.Dependent _name lst -> mapM_ (saveContent . snd) lst
    dbInsert lRule rule

saveVariation :: Variation -> State (AsterixDb Set) ()
saveVariation var = do
    dbInsert lVariation var
    case var of
        Element _o _n rule    -> saveRule rule
        Group lst             -> mapM_ saveItem lst
        Extended lst          -> mapM_ saveItem (catMaybes lst)
        Repetitive _t v       -> saveVariation v
        Explicit _t           -> pure ()
        RandomFieldSequencing -> pure ()
        Compound _t lst       -> mapM_ saveItem (catMaybes lst)

saveItem :: Item -> State (AsterixDb Set) ()
saveItem item = do
    dbInsert lItem item
    case item of
        Spare _o _n           -> pure ()
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
    (AsterixDb mempty mempty mempty mempty mempty mempty mempty mempty)

-- | Enumerated distinct values.
newtype EMap a = EMap { unEMap :: Map a Int }

-- | Enumerated database items. Each Set element gets its sequence number.
enumDb :: AsterixDb Set -> AsterixDb EMap
enumDb db = AsterixDb
    (f dbContent)
    (f dbRule)
    (f dbVariation)
    (f dbItem)
    (f dbUapSel)
    (f dbUap)
    (f dbSpec)
    (f dbAst)
  where
    f :: Ord a => (AsterixDb Set -> Set a) -> EMap a
    f sel = EMap $ Map.fromList $ zip (Set.toAscList $ sel db) [0..]

-- | Get element's index.
indexOf :: Ord a => EMap a -> a -> Int
indexOf (EMap m) = (Map.!) m

enumList :: EMap k -> [(k, Int)]
enumList = Map.toList . unEMap

-- | Variations and Items are defined by mutual recursion.
-- Reorder variations and items, such that any var/item is defined before it's referenced.
flattenVariationsAndItems :: (Set Variation, Set Item) -> [Either Variation Item]
flattenVariationsAndItems = snd . evalRWS go ()
  where
    go = do
        (vars, items) <- get
        case Set.lookupMin vars of
            Just var -> goVar var >> go
            Nothing -> case Set.lookupMin items of
                Just item -> goItem item >> go
                Nothing   -> pure ()
    goVar var = do
        (vars, items) <- get
        put (Set.delete var vars, items)
        when (Set.member var vars) $ do
            case var of
                Element _ _ _         -> pure ()
                Group lst             -> mapM_ goItem lst
                Extended lst          -> mapM_ (maybe (pure ()) goItem) lst
                Repetitive _ var2     -> goVar var2
                Explicit _            -> pure ()
                RandomFieldSequencing -> pure ()
                Compound _ lst        -> mapM_ (maybe (pure ()) goItem) lst
            tell [Left var]
    goItem item = do
        (vars, items) <- get
        put (vars, Set.delete item items)
        when (Set.member item items) $ do
            case item of
                Spare _ _    -> pure ()
                Item _ _ var -> goVar var
            tell [Right item]

-- | Split list of 'Maybe' values to the lists of equal size append 'Nothing'
chunksOf :: Int -> [Maybe a] -> [[Maybe a]]
chunksOf n = unfoldr f
  where
    f [] = Nothing
    f lst =
        let (a,b) = splitAt n lst
        in Just (take n (a <> repeat Nothing), b)

-- | Fspec weight of selected (by name) item
fspecChunkOf :: Num a => A.Name -> [Maybe Item] -> Maybe a
fspecChunkOf name lst = listToMaybe $ do
    (x, i) <- zip [(0::Int)..] (reverse lst)
    item <- maybe empty pure i
    name2 <- case item of
        Spare _ _              -> empty
        Item name2 _title _var -> pure name2
    guard $ name == name2
    pure (2^x)

fspecMaxBytes :: Maybe ByteSize -> [a] -> ByteSize
fspecMaxBytes mn lst = case mn of
    Just m -> case divMod m 8 of    -- no FX
        (n, 0) -> n
        _      -> error "unexpected fx bit size"
    Nothing -> case divMod (length lst) 7 of    -- 1 bit for FX
        (n, 0) -> n
        (n, _) -> succ n

-- | Get 'Fspec' of compound subitem
fspecOf :: Maybe A.RegisterSize -> [Maybe Item] -> A.Name -> Fspec
fspecOf mn lst name = case mn of
    Just _n ->
        let itemGroups = chunksOf 8 $ take (fspecMaxBytes mn lst * 8) (lst <> repeat Nothing)
            fspecs = fspecChunkOf name <$> itemGroups
            results = maybe 0 id <$> fspecs
        in case length (catMaybes fspecs) == 1 of
            False -> error "unexpected fspecs bits"
            True  -> results
    Nothing ->
        -- If a particular FSPEC bit is set,
        -- all FX bits left of this bit must be set too.
        let itemGroups = reverse $ chunksOf 7 lst
            fspecs = fspecChunkOf name <$> itemGroups
            -- Infinite lists of FX flags, once the flag is set,
            -- it remains set forever. It's processed in reverse.
            fxBits = False : zipWith (\a b -> a || isJust b) fxBits fspecs
            results = do
                (mVal, fxFlag) <- zip fspecs fxBits
                let val = maybe 0 (*2) mVal
                    fx = bool 0 1 fxFlag
                pure $ val+fx
        in case length (catMaybes fspecs) == 1 of
            False -> error "unexpected fspecs bits"
            True  -> reverse results

sizeOfVariation :: Variation -> Maybe Int
sizeOfVariation = \case
    Element _o n _cont -> Just n
    Group lst -> fmap (foldr (+) 0) $ sequence $ fmap sizeOfItem lst
    _ -> Nothing

sizeOfItem :: Item -> Maybe Int
sizeOfItem = \case
    Spare _o n -> Just n
    Item _name _title var -> sizeOfVariation var
