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
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import           Data.List               (groupBy, sortOn)
import           Data.Monoid
import           Data.String
import           Data.Word

newtype Datablock t = Datablock { unDatablock :: t }
    deriving (Show, Eq, Functor)

deriving instance IsString (Datablock ByteString)
deriving instance Semigroup (Datablock (Builder, Sum Int))
deriving instance Monoid (Datablock (Builder, Sum Int))

bsToNum :: Num b => ByteString -> b
bsToNum = BS.foldl' (\x w -> x + fromIntegral w) 0

dbCategory :: Num b => Datablock ByteString -> b
dbCategory = bsToNum . BS.take 1 . unDatablock

dbLength :: Num b => Datablock ByteString -> b
dbLength = bsToNum . BS.take 2 . BS.drop 1 . unDatablock

dbData :: Datablock ByteString -> ByteString
dbData = BS.drop 3 . unDatablock

parseDatablock :: ByteString -> Maybe (Datablock ByteString, ByteString)
parseDatablock s = do
    guard $ BS.length s >= 3
    let n = bsToNum $ BS.take 2 $ BS.drop 1 s
    guard $ BS.length s >= n
    let (a, b) = BS.splitAt n s
    pure (Datablock a, b)

parseDatablocks :: ByteString -> Maybe [Datablock ByteString]
parseDatablocks s
    | BS.null s = pure []
    | otherwise = do
          (db, s') <- parseDatablock s
          (db :) <$> parseDatablocks s'

mkDatablock :: Integral cat => cat -> (Builder, Sum Int) -> Datablock (Builder, Sum Int)
mkDatablock cat (records, n) = Datablock
    ( (BB.word8 (fromIntegral cat), 1)
   <> (BB.word8 (fromIntegral n1), 1)
   <> (BB.word8 (fromIntegral n2), 1)
   <> (records, n))
  where
    (n1, n2) = divMod (getSum n + 3) 256

-- | Group records of the same category together, optionally reorder datablocks.
groupRecords :: Bool -> [Datablock ByteString] -> [Datablock (Builder, Sum Int)]
groupRecords allowReorder = fmap combine . groupBy compareCats . shuffle
  where
    shuffle :: [Datablock ByteString] -> [Datablock ByteString]
    shuffle
        | allowReorder = sortOn (\x -> dbCategory x :: Int)
        | otherwise = id

    compareCats :: Datablock ByteString -> Datablock ByteString -> Bool
    compareCats db1 db2 = (dbCategory db1 :: Int) == dbCategory db2

    combine :: [Datablock ByteString] -> Datablock (Builder, Sum Int)
    combine lst = mkDatablock cat (mconcat $ fmap (mkBuilder . dbData) lst)
      where
        cat :: Word8
        cat = dbCategory $ Prelude.head lst
        mkBuilder s = (BB.byteString s, Sum $ BS.length s)
