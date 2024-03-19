
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE LambdaCase           #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

import qualified Data.ByteString as BS
import Data.ByteString.Builder as BL

import Asterix.Base
import Asterix.Generated

mkVariation :: forall t. Reifies t VVariation => Proxy t -> Variation t
mkVariation p = case reflect p of
    VElement o n -> Element $ integerToValue o n 0
    VGroup lst -> Group $ do
        i <- lst
        pure (reify i (mkSome . mkItem))
    VCompound lst -> Compound $ lst >>= \case
        Nothing -> pure Nothing
        Just i -> pure $ Just (reify i (mkSome . mkNonSpare))

mkNonSpare :: forall t. Reifies t VNonSpare => Proxy t -> NonSpare t
mkNonSpare p = case reflect p of
    VNonSpare _name _title var -> NonSpare $ reify var
        (mkSome . mkVariation)

mkItem :: forall t. Reifies t VItem => Proxy t -> Item t
mkItem p = case reflect p of
    VSpare o n -> Spare $ integerToValue o n 0
    VItem nsp -> Item $ reify nsp (mkSome . mkNonSpare)

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
