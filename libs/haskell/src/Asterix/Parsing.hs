-- |
-- Module : Asterix.Parsing

module Asterix.Parsing
( module Asterix.Parsing
, get, put
) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Word

import           Bits

data AsterixError
    = Overflow -- Not enough data available
    | FxError -- Fx bit is set when it is not suppose to be
    | ItemPresenceError -- Item presence indication for non-defined item
    | ExplicitError -- Explicit item size error
    | ValidationError -- Data validation error
    deriving (Show, Eq)

type Parsing = StateT Bits (Either AsterixError)

runParser :: Parsing a -> Bits -> Either AsterixError (a, Bits)
runParser = runStateT

runParser' :: Parsing a -> Bits -> Either AsterixError a
runParser' p x = fst <$> runParser p x

throw :: AsterixError -> Parsing a
throw = lift . Left

eof :: Parsing Bool
eof = fmap Bits.null get

fetch :: Int -> Parsing Bits
fetch n = do
    s <- get
    when (bitLength s < n) $ throw Overflow
    let (a, b) = Bits.splitAt n s
    put b
    pure a

fetchWord8 :: Parsing Word8
fetchWord8 = Bits.indexByte 0 <$> fetch 8

fetchBool :: Parsing Bool
fetchBool = Bits.head <$> fetch 1
