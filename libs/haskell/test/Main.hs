import Test.Tasty
--import Test.Tasty.QuickCheck as QC
--import Test.Tasty.HUnit

import TestBits

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ TestBits.tests
    ]
