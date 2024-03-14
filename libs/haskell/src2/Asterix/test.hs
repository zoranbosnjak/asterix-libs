
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Data.ByteString as BS
import Data.ByteString.Builder as BL

import Asterix.Base
import Asterix.Generated

-- Manually construct items

i0 :: Item TItem_0
i0 = Spare (BitString (BS.singleton 0) (BitOffset 0) (BitSize 8))

i1 :: Item TItem_1
i1 = Item (Element (BitString (BS.pack [0x00, 0x00]) (BitOffset 0) (BitSize 16)))

-- | create variation/item with all bits set to zero

{-
mkZeroItem :: Proxy t -> Some Item
mkZeroItem Proxy = mkSome $ case (reflect @t Proxy) of
    VSpare o n -> Spare undefined
    VItem ... ->
-}

main :: IO ()
main = do

    print i0
    -- TODO: create with mkZero function and type application @TItem_0
    -- TODO: create with mkZero, reify from run-time (manifest !! 0)

    -- Do the same for i1/TItem_1/(manifest !! 1)
    print i1
