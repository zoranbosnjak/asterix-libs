-- | Common code formatting helper functions.

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
nameOfAst (Asterix cat (A.Edition a b) spec) = sformat
    (stext % "_" % left 3 '0' % "_" % int % "_" % int)
    at cat a b
  where
    at = case spec of
             AstCat _ -> "Cat"
             AstRef _ -> "Ref"

fmtList :: Text -> Text -> (a -> Text) -> [a] -> Text
fmtList open close f lst
    = open
    <> mconcat (intersperse ", " $ fmap f lst)
    <> close

div8 :: Integral a => a -> a
div8 n = case divMod n 8 of
    (a, 0) -> a
    _      -> error "unexpected value"
