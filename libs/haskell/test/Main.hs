module Main (main) where

import           Test.Tasty

import qualified TestAsterix
import qualified TestBits
import qualified TestCoding
import qualified TestRawDatablock

tests :: TestTree
tests = testGroup "Tests"
    [ TestBits.tests
    , TestRawDatablock.tests
    , TestAsterix.tests
    , TestCoding.tests
    ]

main :: IO ()
main = defaultMain tests

