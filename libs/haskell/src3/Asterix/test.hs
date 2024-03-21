{-# LANGUAGE DataKinds            #-}

import Data.Maybe (catMaybes)
import Data.Text (Text)

import Asterix.Base
import Asterix.Generated

mkVariation :: forall t. Reifies t VVariation => Proxy t -> Tagged t Item
mkVariation p = Tagged $ case reflect p of
    VElement o n -> Atom $ integerToValue o n 0
    VGroup lst -> ItemList $ do
        i <- lst
        pure (reify i (untag . mkItem))
    VCompound lst -> ItemMap $ do
        i <- catMaybes lst
        pure $ reify i (untag . mkNonSpare)

mkNonSpare :: forall t. Reifies t VNonSpare => Proxy t -> Tagged t (Text, Item)
mkNonSpare p = Tagged $ case reflect p of
    VNonSpare name _title var -> (name, reify var (untag . mkVariation))

mkItem :: forall t. Reifies t VItem => Proxy t -> Tagged t Item
mkItem p = Tagged $ case reflect p of
    VSpare o n -> Atom $ integerToValue o n 0
    VItem nsp -> reify nsp (snd . untag . mkNonSpare)

v3 :: Tagged TVariation_3 Item
v3 = group
    ( item @"SAC" 1
   &: 2
   &: nil)

i3 :: Tagged TNonSpare_2 Item
i3 = nonSpare @"010" v3 -- TODO: use 'item'

{- TODO
v4 :: Tagged TVariation_4 Item
v4 = compound
    ( nonSpare @"010" $ group -- TODO: use 'item'
        ( item @"SAC" 1
       &: 2
       &: nil)
   &: nonSpare @"020" 0 -- TODO: use 'item'
   &: nil)
-}

main :: IO ()
main = do
    print (mkItem (Proxy @TItem_0))
    reify (head items) (print . mkItem)

    print (mkItem (Proxy @TItem_1))
    reify (items !! 1) (print . mkItem)

    print (mkItem (Proxy @TItem_2))
    reify (items !! 2) (print . mkItem)

    print (mkItem (Proxy @TItem_3))
    reify (items !! 3) (print . mkItem)

    print (mkItem (Proxy @TItem_4))
    reify (items !! 4) (print . mkItem)

    print v3
    print i3

    -- print v4
