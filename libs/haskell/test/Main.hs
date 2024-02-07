import Test.Tasty
--import Test.Tasty.QuickCheck as QC
--import Test.Tasty.HUnit

import TestBytes

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ TestBytes.tests
    ]
