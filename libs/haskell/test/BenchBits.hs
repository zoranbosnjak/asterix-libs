{-# LANGUAGE NumericUnderscores #-}

module BenchBits where

{-
import Test.Tasty.Bench
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Bsl
import qualified Data.ByteString.Builder as Bld
import qualified Data.List
import Data.Word
import Data.Maybe

import Bytes
import qualified Asterix.RawDatablock as Db

benchInspect :: Benchmark
benchInspect = bgroup "Inspect test"
    [ bench "sizes only"       $ nf sizesOnly sample
    , bench "using ByteString" $ nf usingByteString sample
    , bench "using Bytes"      $ nf usingBytes sample
    ]
  where
    sample :: ByteString
    sample = mconcat $ replicate 1_000 $
        BS.pack ([0x00, 0x04, 0x03] <> replicate (0x04*256) 0xff)

    sizesOnly :: ByteString -> Int
    sizesOnly = length . fromJust . Db.parseOffsetsAndSizes

    usingByteString :: ByteString -> Int
    usingByteString = length . fromJust . Db.parse

    usingBytes:: ByteString -> Int
    usingBytes = length . fromJust . Db.parse . byteStringToBytes

benchConstruct :: Benchmark
benchConstruct = bgroup "Construct test"
    [ bench "using native Builder" $ nf usingBuilder1 sample
    , bench "using sized Builder"  $ nf usingBuilder2 sample
    , bench "using Bytes"   $ nf usingBytes sample
    , bench "naive"         $ nf naive sample
    ]
  where
    sample :: [Word8]
    sample = replicate 1_000 0

    usingBuilder1 :: [Word8] -> [ByteString]
    usingBuilder1 = Bsl.toChunks . Bld.toLazyByteString . mconcat . fmap Bld.word8

    usingBuilder2 :: [Word8] -> [ByteString]
    usingBuilder2 = sizedBuilderToChunks . mconcat . fmap word8

    usingBytes :: [Word8] -> [ByteString]
    usingBytes = bytesToChunks . mconcat . fmap word8

    naive :: [Word8] -> [ByteString]
    naive = Data.List.singleton . mconcat . fmap BS.singleton
-}

main :: IO ()
main = defaultMain
    [ -- benchInspect
    --, benchConstruct
    ]
