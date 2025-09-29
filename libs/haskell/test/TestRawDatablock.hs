module TestRawDatablock (tests) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder as BSB
import           Data.Either
import           Data.Maybe
import           Data.Word
import           Test.Tasty
import           Test.Tasty.HUnit

import           Asterix.Base
import           Asterix.BitString

allowCategory :: Word8 -> RawDatablock -> Bool
allowCategory n db = rawDatablockCategory db == n

rawDatablockFilter :: (RawDatablock -> Bool) -> ByteString -> Builder
rawDatablockFilter predicate s = case parseRawDatablocks s of
    Left _err -> BSB.byteString s
    Right lst -> mconcat (unparseRawDatablock <$> filter predicate lst)

reverseDatablocks :: ByteString -> Builder
reverseDatablocks s = case parseRawDatablocks s of
    Left _err -> BSB.byteString s
    Right lst -> mconcat (unparseRawDatablock <$> reverse lst)

-- e.g.: received from the network
samples :: [String]
samples =
    [ "01000401" -- cat1
    , "01000401" -- cat1
    , "02000402" -- cat2
    ]

datagramIn :: ByteString
datagramIn = fromJust $ unhexlify $ mconcat samples

tests :: TestTree
tests = testGroup "RawDatablock"
    [ testCase "correct parsing" $ assertEqual "sample"
        True (isRight $ parseRawDatablocks datagramIn)
    , testCase "incorrect parsing" $ assertEqual "sample"
        True (isLeft $ parseRawDatablocks $ BS.tail datagramIn)
    , testCase "filter 1" $ checkFilter 1 cat1
    , testCase "filter 2" $ checkFilter 2 cat2
    , testCase "reverse" $ assertEqual "sample"
        (fromJust $ unhexlify $ mconcat $ reverse samples)
        (builderToByteStringSlow $ reverseDatablocks datagramIn)
    ]
  where
    cat1 = take 2 samples
    cat2 = take 1 $ drop 2 samples
    checkFilter n expected = assertEqual (show n)
        (mconcat expected)
        (hexlify $ builderToByteStringSlow $ rawDatablockFilter (allowCategory n)
            datagramIn)

