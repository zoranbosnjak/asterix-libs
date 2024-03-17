
{-# LANGUAGE DataKinds            #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

import qualified Data.ByteString as BS
import Data.ByteString.Builder as BL

import Asterix.Base
import Asterix.Generated

-- Manually construct items

mkVariation :: forall t. Reifies t VVariation => Variation t
mkVariation = case reflect (Proxy @t) of
    VElement _o _n -> Element
        (BitString (BS.pack [0x00, 0x00]) (BitOffset 0) (BitSize 16))
    VGroup lst -> Group $ do
        i <- lst
        pure (reify i (\(_p :: proxy it) -> mkSome (mkItem @it)))

mkItem :: forall t. Reifies t VItem => Item t
mkItem = case reflect (Proxy @t) of
    VSpare _o _n -> Spare
        (BitString (BS.singleton 0) (BitOffset 0) (BitSize 8))
    VItem _name _title var -> Item $ reify var
        (\(_p :: Proxy vt) -> mkSome (mkVariation @vt))

main :: IO ()
main = do
    print (mkItem @TItem_0)
    reify (head manifest) (\(_p :: Proxy t) -> print (mkItem @t))

    print (mkItem @TItem_1)
    reify (manifest !! 1) (\(_p :: Proxy t) -> print (mkItem @t))

    print (mkItem @TItem_2)
    reify (manifest !! 2) (\(_p :: Proxy t) -> print (mkItem @t))
