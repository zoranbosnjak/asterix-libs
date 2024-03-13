-- |
-- Module : Asterix.Parsing

{-# LANGUAGE DataKinds #-}

-- TODO: remove this
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Asterix.Parsing where

import           GHC.TypeLits
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Bits

import           BitString

data ParserError
    = Overflow -- Not enough data available
    | FxError -- Fx bit is set when it is not suppose to be
    | ItemPresenceError -- Item presence indication for non-defined item
    | ExplicitError -- Explicit item size error
    | ValidationError -- Data validation error
    | RfsError -- Problem with parsing RFS
    | MultipleDatablockError -- Unable to parse multi UAP datablock
    deriving (Show, Eq)

{-
data Parser a c t = forall (b::Nat). Parser (BitString a c -> Either ParserError (t a b, BitString b c))

type Parser = RWST (BitString 0 0) () BitOffset (Either ParserError)

runParser :: Parser a -> ByteString -> BitOffset -> Either ParserError (a, BitOffset)
runParser act s offset = f <$> runRWST act (BitString.fromByteString s) offset
  where
    f (result, offset', ()) = (result, offset')

throw :: ParserError -> Parser a
throw = lift . Left

eof :: Parser Bool
eof = (>=) <$> get <*> asks bitLength

-- | Make sure that n-bits are available, return start of block
fetch :: BitSize -> Parser BitOffset
fetch n = do
    totalSize <- asks snd
    offset1 <- get
    let offset2 = offset1 + n
    when (offset2 > totalSize) $ throw Overflow
    put offset2
    pure offset1

-- | Get word8, assume byte alignment
fetchWord8 :: Parser Word8
fetchWord8 = do
    offset <- fetch 8
    bytes <- asks fst
    pure $ BS.index bytes (div offset 8)

-- | Get bytestring, assume byte alignment
fetchBytes :: Int -> Parser ByteString
fetchBytes requiredBytes = do
    offset <- fetch (requiredBytes * 8)
    bytes <- asks fst
    pure $ BS.take requiredBytes $ BS.drop (div offset 8) bytes

-- | Get single bit
fetchBit :: Parser Bool
fetchBit = do
    offset <- fetch 1
    bytes <- asks fst
    pure $ Data.Bits.testBit (BS.index bytes (div offset 8)) (7 - mod offset 8)

-- | Parsing result.
data Sized a = Sized ByteString BitOffset BitSize a
    deriving (Show, Eq, Functor)

toBits :: Sized a -> SomeBits
toBits (Sized bytes bitOffset n _value) = SomeBits $ Bits bytes bitOffset n

-- | Parse something to Sized.
parseSized :: Parser a -> Parser (Sized a)
parseSized act = do
    offset1 <- get
    result <- act
    offset2 <- get
    bytes <- asks fst
    pure $ Sized bytes offset1 (offset2-offset1) result
-}
