
{-# LANGUAGE DataKinds            #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

import qualified Data.ByteString as BS
import Data.ByteString.Builder as BL

import Asterix.Base
import Asterix.Generated

mkVariation :: forall t. Reifies t VVariation => Proxy t -> Variation t
mkVariation p = case reflect p of
    VElement _o _n -> Element
        (BitString (BS.pack [0x00, 0x00]) (BitOffset 0) (BitSize 16))
    VGroup lst -> Group $ do
        i <- lst
        pure (reify i (mkSome . mkItem))
    VCompound _lst -> Compound $ do
        undefined

mkItem :: forall t. Reifies t VItem => Proxy t -> Item t
mkItem p = case reflect p of
    VSpare _o _n -> Spare
        (BitString (BS.singleton 0) (BitOffset 0) (BitSize 8))
    VItem _name _title var -> Item $ reify var
        (mkSome . mkVariation)

main :: IO ()
main = do
    print (mkItem (Proxy @TItem_0))
    reify (head manifest) (print . mkItem)

    print (mkItem (Proxy @TItem_1))
    reify (manifest !! 1) (print . mkItem)

    print (mkItem (Proxy @TItem_2))
    reify (manifest !! 2) (print . mkItem)

    print (mkItem (Proxy @TItem_3))
    reify (manifest !! 3) (print . mkItem)
