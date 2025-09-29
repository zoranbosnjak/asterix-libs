-- | Generate asterix 'haskell' source code.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell where

import           Control.Monad
import           Control.Monad.Trans.RWS
import           Data.Bool
import           Data.Coerce
import           Data.List               (intersperse, nub, sort)
import           Data.Proxy
import           Data.Set                as Set
import           Data.Text               (Text)
import           Data.Text.Lazy.Builder  (Builder)
import qualified Data.Text.Lazy.Builder  as BL
import           Formatting              as F

import           Asterix.Indent
import qualified Asterix.Specs           as A
import           Struct
import           Types

quote :: Text -> Text
quote = sformat ("\"" % stext % "\"")

fmt :: Format (BlockM Builder ()) a -> a
fmt m = runFormat m line

fmtList :: Text -> Text -> (a -> Text) -> [a] -> Text
fmtList open close f lst
    = open
   <> mconcat (intersperse ", " (fmap f lst))
   <> close

type BB = BlockM Builder ()
type Act = RWS (AsterixDb EMap) [BB] (AsterixDb Set)

out :: a -> RWS r [a] s ()
out = tell . pure

hsType :: Text -> Int -> Act Text -> Act ()
hsType name ix mval = do
    val <- mval
    out $ fmt ("type " % stext % int % " = " % stext) name ix val

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

class RuleArg t where
    ruleArg :: Proxy t -> Text
    ruleFocus :: FocusDb f (A.Rule t)

instance RuleArg A.Content where
    ruleArg _ = "Content"
    ruleFocus = lRuleContent

instance RuleArg (A.Variation OctetOffset) where
    ruleArg _ = "Variation"
    ruleFocus = lRuleVariation

instance (Ord t, Node t, RuleArg t) => Node (A.Rule t) where
    focus = ruleFocus
    node ix rule = hsType (sformat ("TRule" % stext) rArg) ix $ case rule of
        A.ContextFree arg -> do
            ref <- walk arg
            pure $ sformat ("'GContextFree T" % stext % int) rArg ref
        A.Dependent items dv cases -> do
            refDv <- walk dv
            refs <- forM cases $ \(a, b) -> do
                ref <- walk b
                pure $ sformat ("'( " % stext % ", " % stext % ")")
                   (fmtList "'[ " "]" (sformat int) a)
                   (sformat ("T" % stext % int) rArg ref)
            let f2 :: A.ItemName -> Text
                f2 = quote . coerce
                f1 :: A.ItemPath -> Text
                f1 (A.ItemPath lst) = fmtList "'[ " "]" f2 lst
            pure $ sformat ("'GDependent " % stext % " T" % stext % int % " " % stext)
                (fmtList "'[ " "]" f1 items)
                rArg
                refDv
                (fmtList "'[ " "]" id refs)
      where
        rArg = ruleArg (Proxy @t)

convertNumber :: A.Number -> Text
convertNumber = f1 where
    f1 (A.NumInt i) = sformat ("'GNumInt (" % stext % ")") (convertInt i)
    f1 other        = f2 other
    f2 (A.NumPow a b) = sformat ("'GNumPow (" % stext % ") (" % stext % ")")
        (convertInt a)
        (convertInt b)
    f2 other = f3 other
    f3 (A.NumDiv a b) = sformat ("'GNumDiv " % stext % " " % stext)
        (f3 a) (f3 b)
    f3 other = "(" <> f1 other <> ")"
    convertInt i
        | i >= 0 = sformat ("'GZ 'GPlus " % int) i
        | otherwise = sformat ("'GZ 'GMinus " % int) (-i)

instance Node A.Content where
    focus = lContent
    node ix val = hsType "TContent" ix $ case val of
        A.ContentRaw -> do
            pure "'GContentRaw"
        A.ContentTable values -> do
            let f (a, b) = sformat ("'(" % int % ", " % stext % ")") a (quote b)
            pure $ sformat ("'GContentTable " % stext) (fmtList "'[ " "]" f values)
        A.ContentString st -> do
            pure $ sformat ("'GContentString " % stext) $ case st of
                A.StringAscii -> "'GStringAscii"
                A.StringICAO  -> "'GStringICAO"
                A.StringOctal -> "'GStringOctal"
        A.ContentInteger sig _cstr -> do
            pure $ sformat ("'GContentInteger " % stext) $ case sig of
                A.Signed   -> "'GSigned"
                A.Unsigned -> "'GUnsigned"
        A.ContentQuantity sig' lsb unit _cstr -> do
            let sig = case sig' of
                    A.Signed   -> "'GSigned"
                    A.Unsigned -> "'GUnsigned"
            pure $ sformat ("'GContentQuantity " % stext % " " % stext % " " % stext)
                sig ("(" <> convertNumber lsb <> ")") (quote $ coerce unit)
        A.ContentBds bt -> do
            pure $ sformat ("'GContentBds " % stext) $ case bt of
                A.BdsWithAddress -> "'GBdsWithAddress"
                A.BdsAt mn -> case mn of
                    Nothing -> "('GBdsAt 'Nothing)"
                    Just (A.BdsAddr n) -> sformat ("('GBdsAt ('Just " % int % "))") n

instance Node Variation where
    focus = lVariation
    node ix var = hsType "TVariation" ix $ case var of
        A.Element o n rule -> do
            ref <- walk rule
            pure $ sformat ("'GElement " % int % " " % int % " TRuleContent" % int)
                (coerce o :: Int)
                (coerce n :: Int)
                ref
        A.Group o lst -> do
            refList <- mapM walk lst
            pure $ sformat ("'GGroup " % int % " " % stext)
                (coerce o :: Int)
                (fmtList "'[ " "]" (sformat ("TItem" % int)) refList)
        A.Extended lst -> do
            refs <- forM lst $ \case
                Nothing -> pure "'Nothing"
                Just i -> do
                    ref <- walk i
                    pure $ sformat ("'Just TItem" % int) ref
            pure $ sformat ("'GExtended " % stext)
                (fmtList "'[ " "]" id refs)
        A.Repetitive rt var' -> do
            ref <- walk var'
            let rt' = case rt of
                          A.RepetitiveRegular n -> sformat
                              ("('GRepetitiveRegular " % int % ")") (coerce n :: Int)
                          A.RepetitiveFx -> "'GRepetitiveFx"
            pure $ sformat ("'GRepetitive " % stext % " TVariation" % int) rt' ref
        A.Explicit et -> do
            pure $ sformat ("'GExplicit " % stext) $ case et of
                Nothing                  -> "'Nothing"
                Just A.ReservedExpansion -> "('Just 'GReservedExpansion)"
                Just A.SpecialPurpose    -> "('Just 'GSpecialPurpose)"
        A.Compound lst -> do
            refs <- forM lst $ \case
                Nothing -> pure "'Nothing"
                Just nsp -> do
                    ref <- walk nsp
                    pure $ sformat ("'Just TNonSpare" % int) ref
            pure $ sformat ("'GCompound " % stext)
                (fmtList "'[ " "]" id refs)

instance Node Item where
    focus = lItem
    node ix i = hsType "TItem" ix $ case i of
        A.Spare o n -> do
            pure $ sformat ("'GSpare " % int % " " % int)
                (coerce o :: Int)
                (coerce n :: Int)
        A.Item nsp -> do
            ref <- walk nsp
            pure $ sformat ("'GItem TNonSpare" % int) ref

instance Node NonSpare where
    focus = lNonSpare
    node ix (A.NonSpare name title rule _doc) = hsType "TNonSpare" ix $ do
        ref <- walk rule
        pure $ sformat ("'GNonSpare " % stext % " " % stext % " TRuleVariation" % int)
            (quote $ coerce name)
            (quote $ coerce title)
            ref

instance Node UapItem where
    focus = lUapItem
    node ix val = hsType "TUapItem" ix $ case val of
        A.UapItem nsp -> do
            ref <- walk nsp
            pure $ sformat ("'GUapItem TNonSpare" % int) ref
        A.UapItemSpare -> pure "'GUapItemSpare"
        A.UapItemRFS -> pure "'GUapItemRFS"

instance Node Record where
    focus = lRecord
    node ix (Record lst) = hsType "TRecord" ix $ do
        refs <- mapM walk lst
        let f :: Int -> Text
            f = sformat ("TUapItem" % int)
        pure $ sformat ("'GRecord " % stext)
            (fmtList "'[ " "]" f refs)

instance Node Uap where
    focus = lUap
    node ix uap = hsType "TUap" ix $ case uap of
        A.Uap record -> do
            ref <- walk record
            pure $ sformat ("'GUap TRecord" % int) ref
        A.Uaps lst msel -> do
            refs <- forM lst $ \(name, record) -> do
                ref <- walk record
                pure $ sformat ("'(" % stext % ", TRecord" % int % ")")
                  (quote $ coerce name) ref
            let sel = case msel of
                    Nothing -> "'Nothing"
                    Just (A.UapSelector (A.ItemPath p) lst') ->
                        let f :: (Int, A.UapName) -> Text
                            f (a, b) = sformat ("'( " % int % ", " % stext % ")") a
                                (quote $ coerce b)
                        in sformat
                            ("('Just ('GUapSelector " % stext % " " % stext % "))")
                            (fmtList "'[ " "]" (quote . coerce) p)
                            (fmtList "'[ " "]" f lst')
            pure $ sformat ("'GUaps " % stext % " " % stext)
                (fmtList "'[ " "]" id refs) sel

instance Node Expansion where
    focus = lExpansion
    node ix (Expansion mn lst) = hsType "TExpansion" ix $ do
        refs <- forM lst $ \case
            Nothing -> pure "'Nothing"
            Just nsp -> do
                ref <- walk nsp
                pure $ sformat ("'Just TNonSpare" % int) ref
        let sn = case mn of
                Nothing -> "'Nothing"
                Just n -> sformat ("('Just " % int % ")") (coerce n :: Int)
        pure $ sformat ("'GExpansion " % stext % " " % stext)
            sn
            (fmtList "'[ " "]" id refs)

instance Node Asterix where
    focus = lAsterix
    node ix val = hsType "TAsterix" ix $ case val of
        AsterixBasic (A.CatNum cat) (A.Edition a b) uap -> do
            ref <- walk uap
            pure $ sformat ("'GAsterixBasic " % int % " ('GEdition " %
                            int % " " % int % ") TUap" % int) cat a b ref
        AsterixExpansion (A.CatNum cat) (A.Edition a b) expan -> do
            ref <- walk expan
            pure $ sformat ("'GAsterixExpansion " % int % " ('GEdition " %
                            int % " " % int % ") TExpansion" % int) cat a b ref

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
    fmt ("type " % stext % " = TAsterix" % int) (nameOfAst ast) ix

mkManifest :: [Asterix] -> BlockM Builder ()
mkManifest specs = do
    "manifest :: [GAsterix 'ValueLevel]"
    "manifest ="
    indent $ do
        forM_ (zip (["["] <> repeat ",") specs) $ \(delim, spec) -> do
            fmt (stext % " schema @" % stext % " Proxy") delim (nameOfAst spec)
        "]"

-- | Source code generator entry point.
mkCode :: IsTestSpecs -> AstSpecsRef -> AstSpecsDate
        -> CodeGeneratorVersion -> [A.Asterix] -> Builder
mkCode isTestSpecs asRef asDate cgVer specs' = render "    " "\n" $ do
    "-- | Asterix specifications" :: BlockM Builder ()
    ""
    "-- This file is generated, DO NOT EDIT!"
    "-- For more details, see:"
    "--     - https://github.com/zoranbosnjak/asterix-specs"
    ""
    "-- Types are BIG, disable depth checking."
    "{-# OPTIONS_GHC -freduction-depth=0 #-}"
    ""
    "{-# LANGUAGE DataKinds #-}"
    ""
    bool
        "module Asterix.Generated where"
        "module Generated where"
        isTestSpecs
    ""
    "import           Asterix.Schema"
    unless isTestSpecs $ do
        ""
        "asterixSpecsRef :: String"
        line $ "asterixSpecsRef = " <> BL.fromText (quote asRef)
        ""
        "asterixSpecsDate :: String"
        line $ "asterixSpecsDate = " <> BL.fromText (quote asDate)
        ""
        "codeGeneratorVersion :: String"
        line $ "codeGeneratorVersion = " <> BL.fromText (quote cgVer)
    ""
    "-- Asterix types"
    ""
    sequence_ blocks
    ""
    "-- Toplevel aliases"
    ""
    mapM_ mkAlias (zip asterixRefs specs)
    ""
    "-- Manifest"
    ""
    mkManifest specs
    ""
  where
    specs :: [Asterix]
    specs = sort $ nub $ fmap deriveAsterix specs'

    dbSet :: AsterixDb Set
    dbSet = asterixDb specs

    db :: AsterixDb EMap
    db = enumDb dbSet

    (asterixRefs, _, blocks) = runRWS (mapM walk specs) db dbSet
