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
import qualified Data.List as L
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
    | TSymbol Text
    | TList Text Text [T]
    | TProduct Text [T]

instance IsString T where
    fromString = TSymbol . T.pack

-- | Pretty printer
pp :: Bool -> T -> Builder
pp needParen = \case
    TInt i -> bformat int i
    TText t -> "\"" <> bformat stext t <> "\""
    TSymbol t -> bformat stext t
    TList a b lst -> BL.fromText a
        <> mconcat (L.intersperse ", " (fmap (pp False) lst))
        <> BL.fromText b
    TProduct t lst ->
        paren "("
        <> bformat stext t <> " "
        <> mconcat (L.intersperse " " (fmap (pp True) lst))
        <> paren ")"
  where
    paren = flip (bool "") needParen

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
    convert = TList "'[ " "]" . fmap convert

instance (Convert a, Convert b) => Convert (a, b) where
    convert (a, b) = TList "'( " ")" [convert a, convert b]

instance Convert A.Number where
    convert = \case
        A.NumInt i -> TProduct "'TNumInt"
            [ bool "'Minus" "'Plus" (i >= 0)
            , convert (abs i)
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
        A.BdsAt mAddr -> TProduct "'BdsAt"
            [convert $ fmap (coerce @A.BdsAddr @Int) mAddr]

instance Convert A.Content where
    convert = \case
        A.ContentRaw -> "'ContentRaw"
        A.ContentTable lst -> TProduct "'ContentTable" [convert lst]
        A.ContentString t -> TProduct "'ContentString" [fromString ("'" <> show t)]
        A.ContentInteger sig cstr -> TProduct "'ContentInteger"
            [ fromString ("'" <> show sig)
            , convert cstr
            ]
        A.ContentQuantity sig lsb unit cstr -> TProduct "'ContentQuantity"
            [ fromString ("'" <> show sig)
            , convert lsb
            , convert (coerce unit :: Text)
            , convert cstr
            ]
        A.ContentBds t -> TProduct "'ContentBds" [convert t]

data Augmented a b = Augmented Text a b

augment :: Text -> a -> [(b, Int)] -> [(Augmented a b, Int)]
augment name a lst = do
    (x, n) <- lst
    pure (Augmented name a x, n)

instance Convert (Augmented (EMap a) (A.Rule a)) where
    convert (Augmented name mp rule) = case rule of

{-
data Reference a = Reference Text (EMap a) a

mkRef :: (Functor f1, Functor f2)
    => Text -> EMap a -> f1 (f2 a, b) -> f1 (f2 (Reference a), b)
mkRef name mp = fmap act where
    act (x, n) = (fmap (Reference name  mp) x, n)

instance Ord a => Convert (Reference a) where
    convert (Reference target mp x) =
        TSymbol $ sformat ("T" % stext % "_" % int) target (indexOf mp x)

instance Convert a => Convert (A.Rule a) where
    convert = \case
        A.ContextFree x -> TProduct "'TContextFree" [convert x]
        A.Dependent items dv lst -> TProduct "'TDependent"
            [ convert (coerce items :: [[Text]])
            , convert dv
            , convert lst
            ]

{-
instance Convert Variation where
    convert = \case
        A.Element o n rule -> TProduct "'TElement"
            [ convert (coerce o :: Int)
            , convert (coerce n :: Int)
            , convert rule
            ]
        _ -> "()"

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

{-
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
-}
-}

-- | Render line in the form "type T{TEXT}_{INT} = {T}"
mkLine :: Text -> Int -> T -> Builder
mkLine x i t = bformat
    ("type T" % stext % "_" % int % " = " % builder)
    x i (pp False t)

mkType :: Convert t => Text -> (t, Int) -> BlockM Builder ()
mkType name (t, ix) = lineBlockM $ mkLine name ix (convert t)

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
    mapM_ (mkType "Content") (enumList $ dbContent db)
    ""
    "-- | Rule Content set"
    mapM_ (mkType "RuleContent") (augment "Content" (dbContent db) $ enumList $ dbRuleContent db)
    {-
    mapM_ (mkType "RuleContent") (mkRef "Content" (dbContent db) $ enumList $ dbRuleContent db)
    ""
    "-- | Variation set"
    mapM_ (mkType "Variation") (enumList $ dbVariation db)
    sequence_ (fmap (mkType "Variation") $ enumList $ dbVariation db)
    sequence_ (fmap (mkVariation db) $ enumList $ dbVariation db)
    ""
    "-- | Rule Variation set"
    sequence_ (fmap (mkRule "Variation" $ dbVariation db) $ enumList $
        dbRuleVariation db)
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
