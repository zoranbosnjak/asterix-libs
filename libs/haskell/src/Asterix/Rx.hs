{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Asterix.Rx where

import GHC.TypeLits
import Data.Kind
import Data.Word
import Data.Bits
import Data.ByteString as BS
import Data.Proxy
import Data.Maybe
import Unsafe.Coerce
import Control.Monad
import Control.Monad.Fix

import Asterix.Schema

data Some (t :: k -> Type) = forall s. Some (t s)

unSome :: Proxy s -> Some t -> t s
unSome _proxy (Some val) = unsafeCoerce val

type ByteOffset = Int
type BitOffset8 = Int
type BitSize = Int
type ByteSize = Int
data Offset = Offset !ByteOffset !BitOffset8

instance Semigroup Offset where
    Offset a1 b1 <> Offset a2 b2 = Offset c1 c2 where
        (n, c2) = divMod (b1 + b2) 8
        c1 = a1 + a2 + n

instance Monoid Offset where
    mempty = Offset 0 0

instance Eq Offset where
    Offset a1 b1 == Offset a2 b2 = a1 == a2 && b1 == b2

instance Ord Offset where
    compare (Offset a1 b1) (Offset a2 b2) =
        compare a1 a2 <> compare b1 b2

bitOffset :: Int -> Offset
bitOffset n = Offset a b where (a, b) = divMod n 8

bitDelta :: Offset -> Offset -> BitSize
bitDelta (Offset a2 b2) (Offset a1 b1) = o2 - o1 where
    o2 = a2*8+b2
    o1 = a1*8+b1

-- | Asterix RX errors
data Error
    = Overflow

newtype Datablock = Datablock { unDatablock :: ByteString }

dbCategory :: Datablock -> Word8
dbCategory = BS.head . unDatablock

dbRecords :: Datablock -> ByteString
dbRecords = BS.drop 3 . unDatablock

parseDatablock :: ByteString -> Either Error (Datablock, ByteString)
parseDatablock s = do
    (n1, n2) <- case BS.length s < 3 of
        True -> Left Overflow
        False -> Right (BS.index s 1, BS.index s 2)
    let n = fromIntegral n1 * 256 + fromIntegral n2
    case BS.length s < n of
        True -> Left Overflow
        False -> Right (Datablock $ BS.take n s, BS.drop n s)

parseDatablocks :: ByteString -> Either Error [Datablock]
parseDatablocks s
    | BS.null s = Right []
    | otherwise = do
          (a, b) <- parseDatablock s
          (:) <$> pure a <*> parseDatablocks b

-- | Rx monad (reader + state + either) with additional phantom type 't'
newtype Rx t a = Rx (ByteString -> Offset -> Either Error (a, Offset))

instance Functor (Rx t) where
    fmap = undefined

instance Applicative (Rx t) where
    pure = undefined
    (<*>) = undefined

instance Monad (Rx t) where
    (>>=) = undefined

runRx :: (forall t. Rx t a) -> ByteString -> Offset -> Either Error (a, Offset)
runRx (Rx f) = f

err :: Error -> Rx t ()
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
fetchBits mOExpected n = do
    maybe (pure ()) assertBitOffset8 mOExpected
    oCurrent <- get
    oEnd <- endOffset
    let oFinal = oCurrent <> bitOffset n
    when (oFinal > oEnd) $ err Overflow
    put oFinal
    pure oCurrent

fetchBit :: Maybe BitOffset8 -> Rx t Bool
fetchBit mOExpected = do
    Offset o1 o2 <- fetchBits mOExpected 1
    w <- flip BS.index o1 <$> ask
    pure $ testBit w (7 - o2)

fetchByte :: Rx t Word8
fetchByte = do
    assertBitOffset8 0
    Offset o1 _ <- get
    put (Offset (succ o1) 0)
    flip BS.index o1 <$> ask

data Rule b (t :: TRule a) where
    ContextFree :: b -> Rule b ('GContextFree c)
    Dependent :: b -> [b] -> Rule b ('GDependent c d e)

data Variation (t :: TVariation) where
    Element :: !Offset -> !BitSize -> Variation ('GElement o n rule)
    Group :: !Offset -> !BitSize -> [Some Item] -> Variation ('GGroup o lst)
    Extended :: !ByteOffset -> !ByteSize -> [[Some Item]] -> Variation ('GExtended lst)
    Repetitive :: !ByteOffset -> !ByteSize -> [Some Variation] -> Variation ('GRepetitive rt var)
    Explicit :: !ByteOffset -> !ByteSize -> Variation ('GExplicit met)
    Compound :: !ByteOffset -> !ByteSize -> [Some NonSpare] -> Variation ('GCompound lst)

data Item (t :: TItem) where
    Spare :: !Offset -> !BitSize -> Item ('GSpare o n)
    Item :: Some NonSpare -> Item ('GItem nsp)

data NonSpare (t :: TNonSpare) where
    NonSpare :: Some (Rule (Some Variation)) -> NonSpare ('GNonSpare name title vt)

data Record (t :: (TCat, TEdition, [TUapItem])) = Record
    { recOffset :: !ByteOffset
    , recSize   :: !ByteSize
    , recItems  :: [Some NonSpare]
    , recRFS    :: [Some NonSpare]
    }

data Expansion (cat :: TCat) (ed :: TEdition) (fs :: Nat) (t :: [Maybe TNonSpare]) = Expansion
    { expOffset :: !ByteOffset
    , expSize   :: !ByteSize
    , expItems  :: [Some NonSpare]
    }

splitByNothing :: [Maybe a] -> [[a]]
splitByNothing = \case
    [] -> []
    lst ->
        let (a, b) = Prelude.span isJust lst
        in catMaybes a : splitByNothing (Prelude.drop 1 b)

parseFspec :: [Maybe VNonSpare] -> Rx t [VNonSpare]
parseFspec = \case
    [] -> do
        assertBitOffset8 0
        pure []
    Nothing : xs -> do
        fx <- fetchBit (Just 7)
        case fx of
            False -> pure []
            True -> parseFspec xs
    Just nsp : xs -> do
        present <- fetchBit Nothing
        case present of
            False -> parseFspec xs
            True -> (:) <$> pure nsp <*> parseFspec xs

parseVariation :: VVariation -> Rx t (Some Variation)
parseVariation = \case
    GElement oExpected n _rule -> do
        oStart <- fetchBits (Just oExpected) n
        pure $ Some $ Element oStart n
    GGroup _oExpected lst -> do
        oStart <- get
        items <- sequence [parseItem i | i <- lst]
        oEnd <- get
        pure $ Some $ Group oStart (bitDelta oEnd oStart) items
    GExtended lst -> do
        assertBitOffset8 0
        Offset oStart _ <- get
        let lst' :: [[VItem]]
            lst' = splitByNothing lst
            go :: [[VItem]] -> Rx t [[Some Item]]
            go = \case
                [] -> pure []
                (grp : grps) -> do
                    result <- mapM parseItem grp
                    fx <- fetchBit (Just 7)
                    case fx of
                        False -> pure [result]
                        True -> (:) <$> pure result <*> go grps
        items <- go lst'
        assertBitOffset8 0
        Offset oEnd _ <- get
        pure $ Some $ Extended oStart (oEnd - oStart) items
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
                cnt <- go 0 n
                replicateM cnt (parseVariation vVar)
            GRepetitiveFx -> fix $ \loop -> do
                x <- parseVariation vVar
                fx <- fetchBit (Just 7)
                case fx of
                    False -> pure [x]
                    True -> (:) <$> pure x <*> loop
        assertBitOffset8 0
        Offset oEnd _ <- get
        pure $ Some $ Repetitive oStart (oEnd - oStart) result
    GExplicit _ -> do
        assertBitOffset8 0
        Offset oStart _ <- get
        cnt <- fromIntegral <$> fetchByte
        oEnd <- endOffset
        let oFinal = Offset (oStart + cnt) 0
        when (oFinal > oEnd) $ err Overflow
        put oFinal
        pure $ Some $ Explicit oStart cnt
    GCompound lst -> do
        assertBitOffset8 0
        Offset oStart _ <- get
        itemsPresent <- parseFspec lst
        items <- mapM parseNonSpare itemsPresent
        Offset oEnd _ <- get
        pure $ Some $ Compound oStart (oEnd - oStart) items

parseItem :: VItem -> Rx t (Some Item)
parseItem = \case
    GSpare oExpected n -> do
        oStart <- fetchBits (Just oExpected) n
        pure $ Some $ Spare oStart n
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

parseRecord :: [VUapItem] -> Rx t (Some Record)
parseRecord _lst = do
    Offset oStart _ <- get
    Offset oEnd _ <- get
    (lst1, lst2) <- undefined
    pure $ Some $ Record oStart (oEnd - oStart) lst1 lst2
