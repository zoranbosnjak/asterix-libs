-- | Generate asterix 'haskell' source code.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: remove this
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Haskell where

import           Control.Monad
import           Data.Coerce
import           Data.Bool
import           Data.String
import           Data.Text as T
import           Data.List              (nub, sort)
import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import           Formatting             as F

import           Asterix.Indent
import qualified Asterix.Specs          as A
import           Fmt
import           Struct

-- | Intermediate type for pretty printing
data T
    = TInt Integer
    | TText Text
    | TTuple [T]
    | TList [T]
    | TProduct Text [T]

instance IsString T where
    fromString s = TProduct (T.pack s) []

-- | Line gets rendered to: "type T{TEXT}_{INT} = {T}"
data Line = Line Text Int T

class Convert t where
    convert :: t -> T

instance Convert Integer where
    convert = TInt

instance Convert Int where
    convert = TInt . fromIntegral

instance Convert Text where
    convert = TText

instance Convert a => Convert (Maybe a) where
    convert = \case
        Nothing -> "'Nothing"
        Just x -> TProduct "'Just" [convert x]

instance Convert a => Convert [a] where
    convert = TList . fmap convert

instance (Convert a, Convert b) => Convert (a,b) where
    convert (a, b) = TTuple [convert a, convert b]

instance Convert A.Number where
    convert = \case
        A.NumInt i -> TProduct "'TNumInt"
            [ bool "'Minus" "'Plus" (i >= 0)
            , convert i
            ]
        A.NumDiv a b -> TProduct "'TNumDiv"
            [convert a, convert b]
        A.NumPow a b -> TProduct "'TNumPow"
            [convert a, convert b]

instance Convert A.Constrain where
    convert = \case
        A.EqualTo n -> TProduct "'EqualTo" [convert n]
        A.NotEqualTo n -> TProduct "'NotEqualTo" [convert n]
        A.GreaterThan n -> TProduct "'GreaterThan" [convert n]
        A.GreaterThanOrEqualTo n -> TProduct "'GreaterThanOrEqualTo" [convert n]
        A.LessThan n -> TProduct "'LessThan" [convert n]
        A.LessThanOrEqualTo n -> TProduct "'LessThanOrEqualTo" [convert n]

instance Convert A.BdsType where
    convert = \case
        A.BdsWithAddress -> "'BdsWithAddress"
        A.BdsAt mAddr -> TProduct "'BdsAt" [convert $ fmap (coerce @A.BdsAddr @Int) mAddr]

instance Convert A.Content where
    convert = \case
        A.ContentRaw -> "'TContentRaw"
        A.ContentTable lst -> TProduct "'TContentTable" (fmap convert lst)
        A.ContentString t -> TProduct "'TContentString" [fromString ("'" <> show t)]
        A.ContentInteger sig cstr -> TProduct "'TContentInteger"
            [ fromString ("'" <> show sig)
            , TList $ fmap convert cstr
            ]
        A.ContentQuantity sig lsb unit cstr -> TProduct "'TContentQuantity"
            [ fromString ("'" <> show sig)
            , convert lsb
            , convert (coerce unit :: Text)
            , TList $ fmap convert cstr
            ]
        A.ContentBds t -> TProduct "'TContentBds" [convert t]

{-
-- | Helper function to create type level list.
fmtTList :: (a -> Text) -> [a] -> Text
fmtTList = fmtList "'[ " "]"

-- The following functions is a pretty printer trick
-- which follows the shape of the data structure. In this case,
-- the parentheses are on the correct place automatically.
numberToText :: A.Number -> Text
numberToText = f1 where
    f1 = \case
        A.NumInt i -> case i >= 0 of
            True -> sformat ("'TNumInt 'Plus " % int) i
            False -> sformat ("'TNumInt 'Minus " % int) (i * (-1))
        other -> f2 other
    f2 = \case
        A.NumDiv a b -> sformat ("'TNumDiv (" % stext % ") (" % stext % ")") (f1 a) (f1 b)
        other -> f3 other
    f3 = \case
        A.NumPow a b -> sformat ("'TNumPow " % int % " " % int) a b
        other -> f1 other

constrainToText :: A.Constrain -> Text
constrainToText = \case
    A.EqualTo n -> "'( 'EqualTo, " <> numberToText n <> ")"
    A.NotEqualTo n -> "'( 'NotEqualTo, " <> numberToText n <> ")"
    A.GreaterThan n -> "'( 'GreaterThan, " <> numberToText n <> ")"
    A.GreaterThanOrEqualTo n -> "'( 'GreaterThanOrEqualTo, " <> numberToText n <> ")"
    A.LessThan n -> "'( 'LessThan, " <> numberToText n <> ")"
    A.LessThanOrEqualTo n -> "'( 'LessThanOrEqualTo, " <> numberToText n <> ")"

mkContent :: (A.Content, Int) -> BlockM Builder ()
mkContent (cont, ix) = case cont of
    A.ContentRaw -> do
        fmt ("type " % stext % " = 'ContentRaw") t
    A.ContentTable lst -> do
        let f :: (Int, Text) -> Text
            f (a,b) = sformat ("'( " % int % ", \"" % stext % "\")") a b
        fmt ("type " % stext % " = 'ContentTable " % stext) t (fmtTList f lst)
    A.ContentString st -> do
        fmt ("type " % stext % " = 'ContentString " % stext) t $ case st of
            A.StringAscii -> "'StringAscii"
            A.StringICAO  -> "'StringICAO"
            A.StringOctal -> "'StringOctal"
    A.ContentInteger sig cstr -> do
        fmt ("type " % stext % " = 'ContentInteger " % stext % " " % stext) t
            ( case sig of
                 A.Signed   -> "'Signed"
                 A.Unsigned -> "'Unsigned")
            (fmtTList constrainToText cstr)
    A.ContentQuantity sig' lsb' (A.Unit unit) cstr -> do
        let sig = case sig' of
                A.Signed   -> "'Signed"
                A.Unsigned -> "'Unsigned"
        fmt ("type " % stext % " = 'ContentQuantity " % stext
             % " (" % stext % ") \"" % stext % "\" " % stext)
            t
            sig
            (numberToText lsb')
            unit
            (fmtTList constrainToText cstr)
    A.ContentBds bt -> do
        let x = case bt of
                A.BdsWithAddress -> "'BdsWithAddress"
                A.BdsAt Nothing -> "('BdsAt 'Nothing)"
                A.BdsAt (Just (A.BdsAddr addr)) -> sformat
                    ("('BdsAt ('Just " % int % "))") addr
        fmt ("type " % stext % " = 'ContentBds " % stext) t x
  where
    t = nameOf "TContent" ix

mkRule :: Ord a => Text -> EMap a -> (A.Rule a, Int) -> BlockM Builder ()
mkRule name db (rule, ix) = case rule of
    A.ContextFree x -> do
        fmt ("type " % stext % " = 'TContextFree " % stext)
            t
            (nameOf tName $ indexOf db x)
    A.Dependent items' dv lst -> do
        let fText = sformat ("\"" % stext % "\"")
            fLst (xs, cont) = sformat
                ("'( " % stext % ", " % stext % ")")
                (fmtTList (sformat int) xs)
                (nameOf tName $ indexOf db cont)
            items = do
                A.ItemPath p <- items'
                pure [n | A.ItemName n <- p]
        fmt ("type " % stext % " = 'TDependent " % stext % " " % stext % " " % stext)
            t
            (fmtTList (fmtTList fText) items)
            (nameOf tName $ indexOf db dv)
            (fmtTList fLst lst)
  where
    tName = "T" <> name
    t = nameOf ("TRule" <> name) ix

{-
mkVariation :: AsterixDb EMap -> (Variation, Int) -> BlockM Builder ()
mkVariation db (var, ix) = case var of
    Element (OctetOffset o) n rule -> do
        fmt ("type " % stext % " = 'TElement " % int % " " % int % " " % stext)
            t
            o
            n
            (nameOf "TRuleContent" $ indexOf (dbRuleContent db) rule)
    Group lst -> do
        let f :: Item -> Text
            f = nameOf "TItem" . indexOf (dbItem db)
        fmt ("type " % stext % " = 'TGroup " % stext) t
            (fmtTList f lst)
    Extended lst -> do
        let f = \case
                Nothing -> "'Nothing"
                Just item -> "'Just " <> nameOf "TItem" (indexOf (dbItem db) item)
        fmt ("type " % stext % " = 'TExtended " % stext) t
            (fmtTList f lst)
    Repetitive rt' var2' -> do
        let rt = case rt' of
                A.RepetitiveRegular n -> sformat ("('Just " % int % ")") (div8 n)
                A.RepetitiveFx -> "'Nothing"
            var2 = nameOf "TVariation" $ indexOf (dbVariation db) var2'
        fmt ("type " % stext % " = 'TRepetitive " % stext % " " % stext) t rt var2
    Explicit mt -> do
        let met = case mt of
                Nothing -> "'Nothing"
                Just A.ReservedExpansion -> "('Just 'ReservedExpansion)"
                Just A.SpecialPurpose -> "('Just 'SpecialPurpose)"
        fmt ("type " % stext % " = 'TExplicit " % stext) t met
    Compound mn' lst -> do
        let mn = case mn' of
                Nothing -> "'Nothing"
                Just n  -> sformat ("('Just " % int % ")") (div8 n)
            f = \case
                CompoundSubitem item -> "'CompoundSubitem " <> nameOf "TItem" (indexOf (dbItem db) item)
                CompoundSpare -> "'CompoundSpare"
                CompoundRFS -> "'CompoundRFS"
        fmt ("type " % stext % " = 'TCompound " % stext % " " % stext) t mn
            (fmtTList f lst)
  where
    t = nameOf "TVariation" ix

mkItem :: AsterixDb EMap -> (Item, Int) -> BlockM Builder ()
mkItem db (item, ix) = case item of
    Spare (OctetOffset o) n -> do
        fmt ("type " % stext % " = 'TSpare " % int % " " % int) t o n
    Item name title rule -> do
        fmt ("type " % stext % " = 'TItem \"" % stext % "\" \"" %
            stext % "\" " % stext) t name title
            (nameOf "TRuleVariation" $ indexOf (dbRuleVariation db) rule)
  where
    t = nameOf "TItem" ix

mkRecord :: AsterixDb EMap -> (Record, Int) -> BlockM Builder ()
mkRecord db (Record lst, ix) = do
    fmt ("type " % stext % " = 'Record " % stext) t (fmtTList f lst)
  where
    t = nameOf "TRecord" ix
    f = \case
        CompoundSubitem item -> sformat
            ("'CompoundSubitem " % stext)
            (nameOf "TItem" $ indexOf (dbItem db) item)
        CompoundSpare -> "'CompoundSpare"
        CompoundRFS -> "'CompoundRFS"

mkExpansion :: AsterixDb EMap -> (Expansion, Int) -> BlockM Builder ()
mkExpansion db (Expansion n lst, ix) = do
    fmt ("type " % stext % " = 'Expansion " % int % " " % stext)
        t (div8 n) (fmtTList f lst)
  where
    t = nameOf "TExpansion" ix
    f = \case
        CompoundSubitem item -> sformat
            ("'CompoundSubitem " % stext)
            (nameOf "TItem" $ indexOf (dbItem db) item)
        CompoundSpare -> "'CompoundSpare"
        CompoundRFS -> "'CompoundRFS"

mkUap :: AsterixDb EMap -> (Uap, Int) -> BlockM Builder ()
mkUap db (uap, ix) = case uap of
    Uap record -> do
        fmt ("type " % stext % " = 'TUapSingle " % stext) t
            (nameOf "TRecord" $ indexOf (dbRecord db) record)
    Uaps lst _sel -> do
        let f (a, record) = sformat ("'(\"" % stext % "\", " % stext % ")") a
                (nameOf "TRecord" $ indexOf (dbRecord db) record)
        fmt ("type " % stext % " = 'TUapMultiple " % stext) t
            (fmtTList f lst)
  where
    t = nameOf "TUap" ix

mkAstSpec :: AsterixDb EMap -> (AstSpec, Int) -> BlockM Builder ()
mkAstSpec db (astSpec, ix) = do
    let (at, cat, ed, s) = case astSpec of
            AstCat (Cat c) e uap ->
                ("'TCat", c, e, nameOf "TUap" $ indexOf (dbUap db) uap)
            AstRef (Cat c) e expansion ->
                ("'TRef", c, e, nameOf "TExpansion" $ indexOf (dbExpansion db) expansion)
    fmt ("type " % stext % " = " % stext % " " % int % " (" % stext % ") " % stext)
        t at cat (mkEdition ed) s
  where
    t = nameOf "TAstSpec" ix
    mkEdition (A.Edition a b) = sformat
        ("'Edition " % int % " " % int) a b

mkAlias :: (AstSpec, Int) -> BlockM Builder ()
mkAlias (astSpec, ix) = do
    fmt ("type " % stext % " = " % stext)
        (nameOfAst astSpec)
        (nameOf "TAstSpec" ix)

mkManifest :: [AstSpec] -> BlockM Builder ()
mkManifest lst = do
    "manifest :: [Some VAstSpec]"
    "manifest ="
    indent $ do
        forM_ (zip ps lst) $ \(p, astSpec) -> do
            fmt (stext % " schema @" % stext) p (nameOfAst astSpec)
        "]"
  where
    ps = ["["] <> repeat ","
-}

-- | Source code generator entry point.
mkCode :: Bool -> Text -> Text -> [A.Asterix] -> Builder
mkCode test ref ver specs' = render "    " "\n" $ do
    "-- | Asterix specifications" :: BlockM Builder ()
    ""
    "-- This file is generated, DO NOT EDIT!"
    "-- For more details, see:"
    "--    - https://github.com/zoranbosnjak/asterix-specs"
    ""
    "{-# LANGUAGE DataKinds #-}"
    "{-# LANGUAGE OverloadedStrings #-}"
    ""
    "-- Types are BIG, disable depth checking."
    "{-# OPTIONS_GHC -freduction-depth=0 #-}"
    ""
    bool "module Asterix.Generated where" "module Generated where" test
    ""
    "import           Asterix.Schema"
    ""
    "reference :: Text"
    line $ "reference = \"" <> BL.fromText ref <> "\""
    ""
    "version :: Text"
    line $ "version = \"" <> BL.fromText ver <> "\""
    ""
    "-- | Content set"
    sequence_ (fmap mkContent $ enumList $ dbContent db)
    ""
    "-- | Rule Content set"
    sequence_ (fmap (mkRule "Content" $ dbContent db) $ enumList $ dbRuleContent db)
    ""
    {-
    "-- | Variation set"
    sequence_ (fmap (mkVariation db) $ enumList $ dbVariation db)
    ""
    "-- | Rule Variation set"
    sequence_ (fmap (mkRule "Variation" $ dbVariation db) $ enumList $ dbRuleVariation db)
    ""
    "-- | Item set"
    sequence_ (fmap (mkItem db) $ enumList $ dbItem db)
    ""
    "-- | Record set"
    sequence_ (fmap (mkRecord db) $ enumList $ dbRecord db)
    ""
    "-- | Expansion set"
    sequence_ (fmap (mkExpansion db) $ enumList $ dbExpansion db)
    ""
    "-- | Uap set"
    sequence_ (fmap (mkUap db) $ enumList $ dbUap db)
    ""
    "-- | Asterix spec set"
    sequence_ (fmap (mkAstSpec db) $ enumList $ dbAstSpec db)
    ""
    "-- | Aliases"
    sequence_ $ fmap mkAlias $ enumList $ dbAstSpec db
    ""
    "-- | Manifest"
    mkManifest specs
    ""
-}
  where
    specs :: [Asterix]
    specs = sort $ nub $ fmap deriveAsterix specs'

    db :: AsterixDb EMap
    db = enumDb $ asterixDb specs
-}
