{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Asterix.Rx where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Bits
import           Data.ByteString   as BS
import           Data.Coerce
import           Data.Word

import           Asterix.Base
import           Asterix.Schema

-- | Asterix RX errors
data Error
    = Overflow
    | InvalidFrn

-- | Size validated raw datablock
newtype RawDatablock = RawDatablock { unRawDatablock :: ByteString }

rawDbCategory :: RawDatablock -> Word8
rawDbCategory = BS.head . unRawDatablock

rawDbRecords :: RawDatablock -> ByteString
rawDbRecords = BS.drop 3 . unRawDatablock

parseRawDatablock :: ByteString -> Either Error (RawDatablock, ByteString)
parseRawDatablock s = do
    (n1, n2) <- case BS.length s < 3 of
        True  -> Left Overflow
        False -> Right (BS.index s 1, BS.index s 2)
    let n = fromIntegral n1 * 256 + fromIntegral n2
    case BS.length s < n of
        True  -> Left Overflow
        False -> Right (RawDatablock $ BS.take n s, BS.drop n s)

parseRawDatablocks :: ByteString -> Either Error [RawDatablock]
parseRawDatablocks s
    | BS.null s = Right []
    | otherwise = do
          (a, b) <- parseRawDatablock s
          (:) <$> pure a <*> parseRawDatablocks b

-- | Rx monad (reader + state + either) with additional phantom type 't'
newtype Rx t a = Rx (ByteString -> Offset -> Either Error (a, Offset))

runRx :: (forall t. Rx t a) -> ByteString -> Offset -> Either Error (a, Offset)
runRx (Rx f) = f

instance Functor (Rx t) where
    fmap f (Rx act) = Rx (\r s -> do
        (a, o) <- act r s
        pure (f a, o))

instance Applicative (Rx t) where
    pure a = Rx (\_ o -> pure (a, o))
    (Rx mf) <*> (Rx mx) = Rx act where
        act r s = do
            (f, o1) <- mf r s
            (x, o2) <- mx r o1
            pure (f x, o2)

instance Monad (Rx t) where
    Rx mx >>= f = Rx act where
        act r s = do
            (x, o1) <- mx r s
            let Rx act2 = f x
            act2 r o1

err :: Error -> Rx t a
err e = Rx (\_ _ -> Left e)

assertBitOffset8 :: BitOffset8 -> Rx t ()
assertBitOffset8 oExpected = do
    Offset _o1 o2 <- get
    when (oExpected /= o2) $ error "unexpected offset"

ask :: Rx t ByteString
ask = Rx (\r s -> Right (r, s))

get :: Rx t Offset
get = Rx (\_r s -> Right (s, s))

put :: Offset -> Rx t ()
put s = Rx (\_ _ -> Right ((), s))

modify :: (Offset -> Offset) -> Rx t ()
modify f = get >>= put . f

endOffset :: Rx t Offset
endOffset = bitOffset . (* 8) . BS.length <$> ask

fetchBits :: Maybe BitOffset8 -> BitSize -> Rx t Offset
fetchBits mOExpected (BitSize n) = do
    maybe (pure ()) assertBitOffset8 mOExpected
    oCurrent <- get
    oEnd <- endOffset
    let oFinal = oCurrent <> bitOffset n
    when (oFinal > oEnd) $ err Overflow
    put oFinal
    pure oCurrent

fetchBit :: Maybe BitOffset8 -> Rx t Bool
fetchBit mOExpected = do
    Offset (ByteOffset o1) (BitOffset8 o2) <- fetchBits mOExpected 1
    w <- flip BS.index o1 <$> ask
    pure $ testBit w (7 - o2)

fetchByte :: Rx t Word8
fetchByte = do
    assertBitOffset8 0
    Offset o1 _ <- get
    put (Offset (succ o1) 0)
    flip BS.index (coerce o1) <$> ask

data Rule b (t :: TRule a) where
    ContextFree :: b -> Rule b ('GContextFree c)
    Dependent :: b -> [b] -> Rule b ('GDependent c d e)

data Variation (t :: TVariation) where
    Element :: !Offset -> !BitSize -> Variation ('GElement o n rule)
    Group :: !Offset -> !BitSize -> [Some Item] -> Variation ('GGroup o lst)
    Extended :: !ByteOffset -> !ByteSize -> [Maybe (Some Item)] -> Variation ('GExtended lst)
    Repetitive :: !ByteOffset -> !ByteSize -> [Some Variation] -> Variation ('GRepetitive rt var)
    Explicit :: !ByteOffset -> !ByteSize -> Variation ('GExplicit met)
    Compound :: !ByteOffset -> !ByteSize -> [Some NonSpare] -> Variation ('GCompound lst)

data Item (t :: TItem) where
    Spare :: !Offset -> !BitSize -> Item ('GSpare o n)
    Item :: Some NonSpare -> Item ('GItem nsp)

data NonSpare (t :: TNonSpare) where
    NonSpare :: Some (Rule (Some Variation)) -> NonSpare ('GNonSpare name title vt)

data Record (t :: TRecord) = Record
    { recOffset :: !ByteOffset
    , recSize   :: !ByteSize
    , recItems  :: [Some NonSpare]
    , recRFS    :: [Some NonSpare]
    }

data Expansion (t :: TExpansion) = Expansion
    { expOffset :: !ByteOffset
    , expSize   :: !ByteSize
    , expItems  :: [Some NonSpare]
    }

parseFspec :: [Maybe a] -> Rx t [a]
parseFspec = \case
    [] -> do
        assertBitOffset8 0
        pure []
    Nothing : xs -> do
        fx <- fetchBit (Just 7)
        case fx of
            False -> pure []
            True  -> parseFspec xs
    Just nsp : xs -> do
        present <- fetchBit Nothing
        case present of
            False -> parseFspec xs
            True  -> (:) <$> pure nsp <*> parseFspec xs

parseVariation :: VVariation -> Rx t (Some Variation)
parseVariation = \case
    GElement oExpected n _rule -> do
        oStart <- fetchBits (Just $ BitOffset8 oExpected) (BitSize n)
        pure $ Some $ Element oStart (BitSize n)
    GGroup _oExpected lst -> do
        oStart <- get
        items <- sequence [parseItem i | i <- lst]
        oEnd <- get
        pure $ Some $ Group oStart (oEnd `bitDelta` oStart) items
    GExtended lst -> do
        assertBitOffset8 0
        Offset oStart _ <- get
        let go :: [Maybe (GItem String Int)] -> Rx t [Maybe (Some Item)]
            go = \case
                [] -> pure []
                (Nothing : xs) -> (:) <$> pure Nothing <*> go xs
                (Just x : xs) -> (:) <$> (Just <$> parseItem x) <*> go xs
        items <- go lst
        assertBitOffset8 0
        Offset oEnd _ <- get
        pure $ Some $ Extended oStart (oEnd `byteDelta` oStart) items
    GRepetitive t vVar -> do
        assertBitOffset8 0
        Offset oStart _ <- get
        result <- case t of
            GRepetitiveRegular n -> do
                let go :: Int -> ByteSize -> Rx t Int
                    go acc m
                        | m < 0 = error "unexpected value"
                        | m == 0 = pure acc
                        | otherwise = do
                              x <- fromIntegral <$> fetchByte
                              go (acc * 256 + x) (pred m)
                cnt <- go 0 (ByteSize n)
                replicateM cnt (parseVariation vVar)
            GRepetitiveFx -> fix $ \loop -> do
                x <- parseVariation vVar
                fx <- fetchBit (Just 7)
                case fx of
                    False -> pure [x]
                    True  -> (:) <$> pure x <*> loop
        assertBitOffset8 0
        Offset oEnd _ <- get
        pure $ Some $ Repetitive oStart (oEnd `byteDelta` oStart) result
    GExplicit _ -> do
        assertBitOffset8 0
        Offset oStart _ <- get
        cnt <- fromIntegral <$> fetchByte
        oEnd <- endOffset
        let oFinal = Offset (oStart + cnt) 0
        when (oFinal > oEnd) $ err Overflow
        put oFinal
        pure $ Some $ Explicit oStart (coerce cnt)
    GCompound lst -> do
        assertBitOffset8 0
        Offset oStart _ <- get
        itemsPresent <- parseFspec lst
        items <- mapM parseNonSpare itemsPresent
        Offset oEnd _ <- get
        pure $ Some $ Compound oStart (oEnd `byteDelta` oStart) items

parseItem :: VItem -> Rx t (Some Item)
parseItem = \case
    GSpare oExpected n -> do
        oStart <- fetchBits (Just $ BitOffset8 oExpected) (BitSize n)
        pure $ Some $ Spare oStart (BitSize n)
    GItem nsp -> Some . Item <$> parseNonSpare nsp

parseRule :: (x -> Rx t y) -> VRule x -> Rx t (Some (Rule y))
parseRule parser = \case
    GContextFree v -> Some . ContextFree <$> parser v
    GDependent _ v lst -> do
        o <- get
        a <- parser v
        b <- forM lst $ \(_, i) -> do
            put o -- go back with the offset and try each option
            parser i
        pure $ Some $ Dependent a b

parseNonSpare :: VNonSpare -> Rx t (Some NonSpare)
parseNonSpare (GNonSpare _name _title vRule) =
    Some . NonSpare <$> parseRule parseVariation vRule

parseRecord :: forall t. VRecord -> Rx t (Some Record)
parseRecord (GRecord _cat _ed lst) = do
    assertBitOffset8 0
    Offset oStart _ <- get
    itemsPresent <- parseFspec (insertFx 0 lst)
    (lst1, lst2) <- go itemsPresent
    Offset oEnd _ <- get
    pure $ Some $ Record oStart (oEnd `byteDelta` oStart) lst1 lst2
  where
    insertFx :: Int -> [VUapItem] -> [Maybe VUapItem]
    insertFx 7 []       = [Nothing]
    insertFx 7 l        = Nothing : insertFx 0 l
    insertFx n []       = Just GUapItemSpare : insertFx (succ n) []
    insertFx n (x : xs) = Just x : insertFx (succ n) xs

    go :: [VUapItem] -> Rx t ([Some NonSpare], [Some NonSpare])
    go = \case
        [] -> pure ([], [])
        (GUapItem x : xs) -> do
            nsp <- parseNonSpare x
            (lst1, lst2) <- go xs
            pure (nsp : lst1, lst2)
        (GUapItemSpare : xs) -> go xs
        (GUapItemRFS : xs) -> do
            lstRfs <- do
                assertBitOffset8 0
                cnt <- fromIntegral <$> fetchByte
                replicateM cnt parseRfs
            (lst1, lst2) <- go xs
            pure (lst1, lstRfs <> lst2)

    parseRfs :: Rx t (Some NonSpare)
    parseRfs = do
        assertBitOffset8 0
        frn <- fromIntegral <$> fetchByte
        -- first valid 'frn' is 1
        when (frn <= 0 || frn > Prelude.length lst) $ err InvalidFrn
        case lst !! pred frn of
            GUapItem nsp  -> parseNonSpare nsp
            GUapItemSpare -> err InvalidFrn
            GUapItemRFS   -> err InvalidFrn
