module Main (main) where

import           Test.Tasty

import qualified TestAsterix
import qualified TestBits
import qualified TestRawDatablock
import qualified TestCoding

tests :: TestTree
tests = testGroup "Tests"
    [ TestBits.tests
    , TestRawDatablock.tests
    , TestAsterix.tests
    , TestCoding.tests
    ]

main :: IO ()
main = defaultMain tests

