-- | Generate asterix 'python' source code.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Python where

import           Control.Monad
import           Control.Monad.Trans.RWS
import           Data.List              (intersperse, nub, sort, sortOn)
import           Data.Maybe
import           Data.Scientific
import           Data.Coerce
import           Data.Set               as Set
import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import           Formatting             as F

import           Asterix.Indent
import qualified Asterix.Specs          as A
import           Struct

quote :: Text -> Text
quote = sformat ("\"" % stext % "\"")

fmt :: Format (BlockM Builder ()) a -> a
fmt m = runFormat m line

fmtList :: Text -> Text -> (a -> Text) -> [a] -> Text
fmtList open close f lst
    = open
   <> mconcat (intersperse ", " (fmap f lst))
   <> close

alias :: Text -> Text -> BlockM Builder ()
alias = fmt (stext % " : TypeAlias = " % stext)

pyClass :: (Integral a, HasIndent (BlockM Builder b))
    => Text -> a -> Text -> BlockM Builder b -> BlockM Builder b
pyClass name ix baseClass' body = do
    fmt ("class " % stext % "_" % int % stext % ":")
        name ix baseClass
    indent body
  where
    baseClass = case baseClass' of
        "" -> ""
        _ -> sformat ("(" % stext % ")") baseClass'

type BB = BlockM Builder ()
type Act = RWS (AsterixDb EMap) [BB] (AsterixDb Set)

out :: a -> RWS r [a] s ()
out = tell . pure

class Node t where
    focus :: FocusDb f t
    node :: Int -> t -> Act ()

walk :: forall t. (Ord t, Node t) => t -> Act Int
walk t = do
    db1 <- ask
    db2 <- get
    let ix = indexOf (getDb (focus @t) db1) t
    when (Set.member t (getDb (focus @t) db2)) $ do
        put $ setDb (focus @t) db2 (Set.delete t (getDb (focus @t) db2))
        node ix t
    pure ix

instance Node A.Content where
    focus = lContent
    node ix = \case
        A.ContentRaw -> do
            out $ pyClass "Content" ix "ContentRaw" $ do
                "pass"
        A.ContentTable values -> do
            let f (a, b) = sformat (int % ": " % stext) a (quote b)
            out $ pyClass "Content" ix "ContentTable" $ do
                fmt ("values = " % stext) (fmtList "{" "}" f values)
        A.ContentString st -> do
            out $ pyClass "Content" ix "ContentString" $ do
                fmt ("string_type = " % stext) $ case st of
                    A.StringAscii -> "StringAscii"
                    A.StringICAO  -> "StringICAO"
                    A.StringOctal -> "StringOctal"
        A.ContentInteger sig _cstr -> do
            out $ pyClass "Content" ix "ContentInteger" $ do
                fmt ("signedness = " % stext) $ case sig of
                    A.Signed -> "Signed"
                    A.Unsigned -> "Unsigned"
        A.ContentQuantity sig lsb unit _cstr -> do
            out $ pyClass "Content" ix "ContentQuantity" $ do
                fmt ("signedness = " % stext) $ case sig of
                    A.Signed -> "Signed"
                    A.Unsigned -> "Unsigned"
                fmt ("lsb = " % scifmt Generic Nothing)
                    (fromFloatDigits (evalNumber lsb :: Double))
                fmt ("unit = " % stext) (quote $ coerce unit)
        A.ContentBds bt -> do
            out $ pyClass "Content" ix "ContentBds" $ do
                case bt of
                    A.BdsWithAddress -> "bds_type = BdsWithAddress"
                    A.BdsAt Nothing -> "bds_type = (BdsAt, None)"
                    A.BdsAt (Just addr) -> fmt ("bds_type = (BdsAt, " % int % ")")
                        (coerce addr :: Int)

instance Node (A.Rule A.Content) where
    focus = lRuleContent
    node ix = \case
        A.ContextFree content -> do
            ref <- walk content
            out $ pyClass "RuleContent" ix "RuleContentContextFree" $ do
                fmt ("variation = Content_" % int) ref
        A.Dependent items dv cases -> do
            refDv <- walk dv
            refs <- forM cases $ \(a, b) -> do
                let val = fmtList "[" "]" (sformat int) a
                ref <- walk b
                pure $ fmt ("(" % stext % ", Content_" % int % "),") val ref
            out $ pyClass "RuleContent" ix "RuleContentDependent" $ do
                let f1 (A.ItemPath path) = fmtList "[" "]" f2 path
                    f2 (A.ItemName name) = quote name
                fmt ("depends_on = " % stext)
                    (fmtList "[" "]" f1 items)
                fmt ("default_variation = Content_" % int) refDv
                "cases = ["
                indent $ sequence_ refs
                "]"

instance Node Variation where
    focus = lVariation
    node ix = \case
        A.Element o n rule -> do
            ref <- walk rule
            out $ pyClass "Variation" ix "Element" $ do
                fmt ("bit_offset8 = " % int) (coerce o :: Int)
                fmt ("bit_size = " % int) (coerce n :: Int)
                fmt ("rule = RuleContent_" % int) ref
        A.Group lst -> do
            refList <- mapM walk lst
            refDict <- fmap catMaybes $ forM lst $ \case
                A.Spare _ _ -> pure Nothing
                A.Item nsp@(A.NonSpare name _ _ _) -> do
                    ref <- walk nsp
                    pure $ Just (name, ref)
            let n = sum $ fmap bitSizeOfItem lst
            out $ pyClass "Variation" ix "Group" $ do
                fmt ("bit_size = " % int) n
                fmt ("items_list = " % stext)
                    (fmtList "[" "]" (sformat ("Item_" % int)) refList)
                let f (name, ref) = sformat (stext % ": NonSpare_" % int)
                        (quote $ coerce name) ref
                fmt ("items_dict = " % stext) (fmtList "{" "}" f refDict)
        A.Extended lst -> do
            refs <- forM lst $ \case
                Nothing -> pure "None"
                Just i -> sformat ("Item_" % int) <$> walk i
            out $ pyClass "Variation" ix "Extended" $ do
                fmt ("items = " % stext) (fmtList "[" "]" id refs)
        A.Repetitive rt var -> do
            ref <- walk var
            out $ pyClass "Variation" ix "Repetitive" $ do
                case rt of
                    A.RepetitiveFx -> "rep_bytes = None"
                    A.RepetitiveRegular n -> do
                        fmt ("rep_bytes = " % int) (coerce n :: Int)
                fmt ("variation = Variation_" % int) ref
        A.Explicit met -> do
            out $ pyClass "Variation" ix "Explicit" $ do
                fmt ("explicit_type = " % stext) $ case met of
                    Nothing -> "None"
                    Just A.ReservedExpansion -> "ReservedExpansion"
                    Just A.SpecialPurpose -> "SpecialPurpose"
        A.Compound lst -> do
            refList <- forM lst $ \case
                Nothing -> pure "None"
                Just i -> sformat ("NonSpare_" % int) <$> walk i
            refDict <- forM (catMaybes lst) $ \nsp -> case nsp of
                (A.NonSpare name _ _ _) -> (name,) <$> walk nsp
            out $ pyClass "Variation" ix "Compound" $ do
                fmt ("items_list = " % stext) (fmtList "[" "]" id refList)
                let f (A.ItemName name, i) = sformat
                        (stext % ": NonSpare_" % int)
                        (quote name) i
                fmt ("items_dict = " % stext) (fmtList "{" "}" f refDict)

instance Node (A.Rule Variation) where
    focus = lRuleVariation
    node ix = \case
        A.ContextFree var -> do
            ref <- walk var
            out $ pyClass "RuleVariation" ix "RuleVariationContextFree" $ do
                fmt ("variation = Variation_" % int) ref
        A.Dependent items dv cases -> do
            refDv <- walk dv
            refs <- forM cases $ \(a, b) -> do
                let val = fmtList "[" "]" (sformat int) a
                ref <- walk b
                pure $ fmt ("(" % stext % ", Variation_" % int % "),") val ref
            out $ pyClass "RuleVariation" ix "RuleVariationDependent" $ do
                let f1 (A.ItemPath path) = fmtList "[" "]" f2 path
                    f2 (A.ItemName name) = quote name
                fmt ("depends_on = " % stext)
                    (fmtList "[" "]" f1 items)
                fmt ("default_variation = Variation_" % int) refDv
                "cases = ["
                indent $ sequence_ refs
                "]"

instance Node NonSpare where
    focus = lNonSpare
    node ix (A.NonSpare name title rule _doc) = do
        ref <- walk rule
        out $ pyClass "NonSpare" ix "NonSpare" $ do
            fmt ("name = " % stext) (quote $ coerce name)
            fmt ("title = " % stext) (quote $ coerce title)
            fmt ("rule = RuleVariation_" % int) ref

instance Node Item where
    focus = lItem
    node ix = \case
        A.Spare o n -> do
            out $ pyClass "Item" ix "Spare" $ do
                fmt ("bit_offset8 = " % int) (coerce o :: Int)
                fmt ("bit_size = " % int) (coerce n :: Int)
        A.Item nsp -> do
            ref <- walk nsp
            out $ pyClass "Item" ix "Item" $ do
                fmt ("non_spare = NonSpare_" % int) ref

instance Node Record where
    focus = lRecord
    node ix (Record items) = do
        refs <- forM items $ \case
                A.UapItem x -> sformat ("NonSpare_" % int) <$> walk x
                A.UapItemSpare -> pure "UapItemSpare"
                A.UapItemRFS -> pure "UapItemRFS"
        out $ pyClass "Record" ix "Record" $ do
            fmt ("items = " % stext) (fmtList "[" "]" id refs)

instance Node Uap where
    focus = lUap
    node ix = \case
        A.Uap record -> do
            ref <- walk record
            out $ pyClass "Uap" ix "UapSingle" $ do
                fmt ("record = Record_" % int) ref
        A.Uaps lst msel -> do
            refs <- forM lst $ \(A.UapName uapName, record) -> do
                ref <- walk record
                pure $ sformat (stext % ": Record_" % int) (quote uapName) ref
            out $ pyClass "Uap" ix "UapMultiple" $ do
                let f (i, A.UapName uapName) = sformat
                        (int % ": " % stext)
                        i (quote uapName)
                fmt ("uaps = " % stext) (fmtList "{" "}" id refs)
                fmt ("selector = " % stext) $ case msel of
                    Nothing -> "None"
                    Just (A.UapSelector (A.ItemPath item) table) ->
                        "("
                     <> fmtList "[" "]" quote (coerce item)
                     <> ", "
                     <> fmtList "{" "}" f table
                     <> ")"

instance Node Expansion where
    focus = lExpansion
    node ix (Expansion n lst) = do
        refs <- forM lst $ \case
            Nothing -> pure "None"
            Just nsp -> sformat ("NonSpare_" % int) <$> walk nsp
        out $ pyClass "Expansion" ix "Expansion" $ do
            fmt ("fspec_bytes = " % int) (coerce n :: Int)
            fmt ("items = " % stext) (fmtList "[" "]" id refs)

instance Node Asterix where
    focus = lAsterix
    node ix = \case
        AsterixBasic cat (A.Edition a b) uap -> do
            ref <- walk uap
            out $ pyClass "Asterix" ix "AstCat" $ do
                fmt ("category = " % int) (coerce cat :: Int)
                fmt ("edition = (" % int % ", " % int % ")") a b
                fmt ("uap = Uap_" % int) ref
        AsterixExpansion cat (A.Edition a b) expan -> do
            ref <- walk expan
            out $ pyClass "Asterix" ix "AstRef" $ do
                fmt ("category = " % int) (coerce cat :: Int)
                fmt ("edition = (" % int % ", " % int % ")") a b
                fmt ("expansion = Expansion_" % int) ref

nameOfAst :: Asterix -> Text
nameOfAst = \case
    AsterixBasic cat ed _ -> f "Cat" cat ed
    AsterixExpansion cat ed _ -> f "Ref" cat ed
  where
    f astType (A.CatNum cat) (A.Edition a b) = sformat
        (stext % "_" % left 3 '0' % "_" % int % "_" % int)
        astType cat a b

mkAlias :: (Int, Asterix) -> BlockM Builder ()
mkAlias (ix, ast) = do
    alias (nameOfAst ast) (sformat ("Asterix_" % int) ix)

mkManifest :: [Asterix] -> BlockM Builder ()
mkManifest specs = do
    "manifest = {"
    indent $ do
        "'CATS': {"
        indent $ mconcat $ go "CAT"
        "},"
        "'REFS': {"
        indent $ mconcat $ go "REF"
        "},"
    "}"
  where
    lst = do
        spec <- specs
        pure $ case spec of
            AsterixBasic cat ed _ -> ("CAT", (cat, (ed, nameOfAst spec)))
            AsterixExpansion cat ed _ -> ("REF", (cat, (ed, nameOfAst spec)))

    go :: Text -> [BlockM Builder ()]
    go t = do
        let candidates = [b | (a, b) <- lst, a == t]
        cat <- sort $ nub $ fmap fst candidates
        let hdr = fmt (int % ": {") (coerce cat :: Int)
        pure $ do
            hdr
            indent $ mconcat $ do
                (A.Edition eMaj eMin, cls) <- sortOn fst
                    [b | (a, b) <- candidates, a == cat]
                let edition = sformat ("'" % int % "." % int % "'") eMaj eMin
                pure $ fmt stext ( edition <> ": " <> cls <> ",")
            "},"

-- | Source code generator entry point.
mkCode :: Bool -> Text -> Text -> [A.Asterix] -> Builder
mkCode _testSpecs ref ver specs' = render "    " "\n" $ do
    "# Asterix specifications" :: BlockM Builder ()
    ""
    "# This file is generated, DO NOT EDIT!"
    "# For more details, see:"
    "#     - https://github.com/zoranbosnjak/asterix-specs"
    ""
    "from asterix.base import *"
    ""
    line $ "reference = " <> BL.fromText (quote ref)
    line $ "version = " <> BL.fromText (quote ver)
    ""
    "# Asterix types"
    ""
    sequence_ (intersperse "" blocks)
    ""
    "# Aliases"
    ""
    mapM_ mkAlias (zip asterixRefs specs)
    ""
    "# Manifest"
    ""
    mkManifest specs
  where
    specs :: [Asterix]
    specs = sort $ nub $ fmap deriveAsterix specs'

    dbSet :: AsterixDb Set
    dbSet = asterixDb specs

    db :: AsterixDb EMap
    db = enumDb dbSet

    (asterixRefs, _, blocks) = runRWS (mapM walk specs) db dbSet
