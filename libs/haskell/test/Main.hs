import Test.Tasty

import TestBits
import TestDatablock
import TestGeneric
import TestSpecific

main :: IO ()
main = defaultMain $ testGroup "Asterix tests"
    [ TestBits.tests
    , TestDatablock.tests
    , TestGeneric.tests
    , TestSpecific.tests
    ]
