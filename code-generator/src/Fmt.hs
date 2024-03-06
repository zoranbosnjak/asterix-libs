-- | Common code formatting helper functions.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Fmt where

import           Data.List              (intersperse)
import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             as F

import           Asterix.Indent
import qualified Asterix.Specs          as A
import           Struct

-- | The same as 'line $ bformat (formating) arg1 arg2 ...'
fmt :: Format (BlockM Builder ()) a -> a
fmt m = runFormat m line

nameOf :: Integral a => Text -> a -> Text
nameOf = sformat (stext % "_" % int)

nameOfAst :: Asterix -> Text
nameOfAst = \case
    AsterixBasic cat ed _ -> f "Cat" cat ed
    AsterixExpansion cat ed _ -> f "Ref" cat ed
  where
    f astType (A.CatNum cat) (A.Edition a b) = sformat
        (stext % "_" % left 3 '0' % "_" % int % "_" % int)
        astType cat a b

fmtList :: Text -> Text -> (a -> Text) -> [a] -> Text
fmtList open close f lst
    = open
    <> mconcat (intersperse ", " $ fmap f lst)
    <> close

div8 :: Integral a => a -> a
div8 n = case divMod n 8 of
    (a, 0) -> a
    _      -> error "unexpected value"
