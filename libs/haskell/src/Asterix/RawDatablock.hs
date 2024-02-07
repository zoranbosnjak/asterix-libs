-- | Raw datablock manipulation routines.
--
-- Raw datablock is the first level of asterix in the form
-- [cat|len|records...|cat|len|records...|...]
-- where
--    'cat' is 1 octet long
--    'len' is 2 octet long
--    'records' represents the actual bytes of data

module Asterix.RawDatablock where

import           Control.Monad

import           Asterix.Parsing
import           Bits
import           BitsBuilder

newtype Datablock bs = Datablock { unDatablock :: bs }
    deriving (Show, Eq, Functor)

dbCategory :: Num b => Datablock Bits -> b
dbCategory = Bits.getNumberAligned . Bits.take 8 . unDatablock

dbLength :: Num b => Datablock Bits -> b
dbLength = Bits.getNumberAligned . Bits.take 16 . Bits.drop 8 . unDatablock

dbData :: Datablock Bits -> Bits
dbData = Bits.drop 24 . unDatablock

parseDatablock :: Parsing (Datablock Bits)
parseDatablock = do
    s <- get
    _cat <- fetchWord8
    n <- (* 8) . Bits.getNumberAligned <$> fetch 16
    when (Bits.length s < n) $ throw Overflow
    let (a,b) = Bits.splitAt n s
    put b
    pure $ Datablock a

parseDatablocks :: Parsing [Datablock Bits]
parseDatablocks = eof >>= \case
    True -> pure []
    False -> (:) <$> parseDatablock <*> parseDatablocks


mkDatablock :: Integral cat => cat -> BitsBuilder -> Datablock BitsBuilder
mkDatablock cat records = Datablock
    ( word8 (fromIntegral cat)
   <> word8 (fromIntegral n1)
   <> word8 (fromIntegral n2)
   <> records)
  where
    n = div (BitsBuilder.length records) 8
    (n1, n2) = divMod (n + 3) 256
