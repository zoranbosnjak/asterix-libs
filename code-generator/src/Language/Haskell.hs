-- | Generate asterix 'haskell' source code.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell where

import           Control.Monad
import           Data.Bool
import           Data.Coerce
import           Data.List              (nub, sort)
import qualified Data.List              as L
import           Data.String
import           Data.Text              as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import           Formatting             as F

import           Asterix.Indent
import qualified Asterix.Specs          as A
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

-- | Pretty printer for 'T'
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

instance Convert A.RepetitiveType where
    convert = \case
        A.RepetitiveRegular n -> TProduct "'Just"
            [convert (coerce n :: Int)]
        A.RepetitiveFx -> "'Nothing"

instance Convert A.ExplicitType where
    convert = \case
        A.ReservedExpansion -> "'ReservedExpansion"
        A.SpecialPurpose -> "'SpecialPurpose"

instance Convert (Augmented A.Content) where
    convert (Augmented _db content) = case content of
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

newtype Symbol = Symbol Text
    deriving (Eq, Ord)

instance Convert Symbol where
    convert = TSymbol . coerce

refName :: Ord a => Text -> EMap a -> a -> Text
refName name mp x = sformat ("T" % stext % "_" % int) name (indexOf mp x)

-- | In some cases, we need database object in convert instance.
data Augmented t = Augmented (AsterixDb EMap) t

instance Convert a => Convert (A.Rule a) where
    convert = \case
        A.ContextFree x -> TProduct "'TContextFree" [convert x]
        A.Dependent items dv lst -> TProduct "'TDependent"
            [ convert (coerce items :: [[Text]])
            , convert dv
            , convert lst
            ]

instance Convert (Augmented (A.Rule A.Content)) where
    convert (Augmented db rule) =
        convert (fmap (Symbol . refName "Content" (dbContent db)) rule)

instance Convert (Augmented Variation) where
    convert (Augmented db var) = case var of
        A.Element o n rule -> TProduct "'TElement"
            [ convert (coerce o :: Int)
            , convert (coerce n :: Int)
            , convert (refRule rule)
            ]
        A.Group lst -> TProduct "'TGroup"
            [ convert $ fmap refItem lst
            ]
        A.Extended lst -> TProduct "'TExtended"
            [ convert $ fmap (fmap refItem) lst
            ]
        A.Repetitive rt var2 -> TProduct "'TRepetitive"
            [ convert rt
            , convert $ refVariation var2
            ]
        A.Explicit met -> TProduct "'TExplicit"
            [ convert met
            ]
        A.Compound lst -> TProduct "'TCompound"
            [ convert $ fmap (fmap refItem) lst
            ]
      where
        refRule = Symbol . refName "RuleContent" (dbRuleContent db)
        refItem = Symbol . refName "Item" (dbItem db)
        refVariation = Symbol . refName "Variation" (dbVariation db)

instance Convert (Augmented (A.Rule Variation)) where
    convert (Augmented db rule) =
        convert (fmap (Symbol . refName "Variation" (dbVariation db)) rule)

instance Convert (Augmented Item) where
    convert (Augmented db item) = case item of
        A.Spare o n -> TProduct "'TSpare"
            [ convert (coerce o :: Int)
            , convert (coerce n :: Int)
            ]
        A.Item iName title rule _doc -> TProduct "'TItem"
            [ convert (coerce iName :: Text)
            , convert (coerce title :: Text)
            , convert (refRule rule)
            ]
      where
        refRule = Symbol . refName "RuleVariation" (dbRuleVariation db)

instance Convert (Augmented UapItem) where
    convert (Augmented db uapItem) = case uapItem of
        A.UapItem item -> TProduct "'UapItem"
            [ convert $ refItem item
            ]
        A.UapItemSpare -> "'UapItemSpare"
        A.UapItemRFS -> "'UapItemRFS"
      where
        refItem = Symbol . refName "Item" (dbItem db)

instance Convert (Augmented Record) where
    convert (Augmented db (Record lst)) = TProduct "'Record"
        [ convert $ fmap (Augmented db) lst]

instance Convert (Augmented Expansion) where
    convert (Augmented db (Expansion n lst)) = TProduct "'Expansion"
        [ convert (coerce n :: Int)
        , convert $ fmap (fmap refItem) lst
        ]
      where
        refItem = Symbol . refName "Item" (dbItem db)

instance Convert A.UapName where
    convert n = convert (coerce n :: Text)

instance Convert a => Convert (A.Uap a) where
    convert = \case
        A.Uap t -> TProduct "'TUapSingle"
            [ convert t
            ]
        A.Uaps lst _msel -> TProduct "'TUapMultiple"
            [ convert lst
            ]

instance Convert (Augmented Uap) where
    convert (Augmented db uap) =
        convert (fmap (Symbol . refName "Record" (dbRecord db)) uap)

instance Convert A.Edition where
    convert (A.Edition a b) = TProduct "'Edition"
        [ convert a
        , convert b
        ]

instance Convert (Augmented Asterix) where
    convert (Augmented db asterix) = case asterix of
        AsterixBasic catNum edition uap -> TProduct "'TBasic"
            [ convert (coerce catNum :: Int)
            , convert edition
            , convert (Symbol $ refName "Uap" (dbUap db) uap)
            ]
        AsterixExpansion catNum edition expansion -> TProduct "'TExpansion"
            [ convert (coerce catNum :: Int)
            , convert edition
            , convert (Symbol $ refName "Expansion" (dbExpansion db) expansion)
            ]

nameOfAst :: Asterix -> Text
nameOfAst = \case
    AsterixBasic cat ed _ -> f "Cat" cat ed
    AsterixExpansion cat ed _ -> f "Ref" cat ed
  where
    f astType (A.CatNum cat) (A.Edition a b) = sformat
        (stext % "_" % left 3 '0' % "_" % int % "_" % int)
        astType cat a b

mkAlias :: AsterixDb EMap -> (Asterix, Int) -> BlockM Builder ()
mkAlias db (asterix, _ix) = lineBlockM $ bformat
    ("type " % stext % " = " % stext)
    (nameOfAst asterix)
    (refName "Asterix" (dbAsterix db) asterix)

mkManifest :: [Asterix] -> BlockM Builder ()
mkManifest lst = do
    "manifest :: [Some VAsterix]"
    "manifest ="
    indent $ do
        forM_ (Prelude.zip ps lst) $ \(p, astSpec) -> do
            fmt (stext % " schema @" % stext) p (nameOfAst astSpec)
        "]"
  where
    ps = ["["] <> repeat ","
    fmt m = runFormat m line

-- | Render lines in the form "type T{TEXT}_{INT} = {T}"
mkTypes :: Convert (Augmented t) => AsterixDb EMap -> Text -> EMap t
    -> BlockM Builder ()
mkTypes db name mp = mapM_ go (enumList mp) where
    go (t, ix) = lineBlockM $ bformat
        ("type T" % stext % "_" % int % " = " % builder)
        name ix (pp False $ convert (Augmented db t))

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
    mkTypes db "Content" (dbContent db)
    ""
    "-- | Rule Content set"
    mkTypes db "RuleContent" (dbRuleContent db)
    ""
    "-- | Variation set"
    mkTypes db "Variation" (dbVariation db)
    ""
    "-- | Rule Variation set"
    mkTypes db "RuleVariation" (dbRuleVariation db)
    ""
    "-- | Item set"
    mkTypes db "Item" (dbItem db)
    ""
    "-- | Record set"
    mkTypes db "Record" (dbRecord db)
    ""
    "-- | Expansion set"
    mkTypes db "Expansion" (dbExpansion db)
    ""
    "-- | Uap set"
    mkTypes db "Uap" (dbUap db)
    ""
    "-- | Asterix spec set"
    mkTypes db "Asterix" (dbAsterix db)
    ""
    "-- | Aliases"
    mapM_ (mkAlias db) $ enumList $ dbAsterix db
    ""
    "-- | Manifest"
    mkManifest specs
    ""
  where
    specs :: [Asterix]
    specs = sort $ nub $ fmap deriveAsterix specs'

    db :: AsterixDb EMap
    db = enumDb $ asterixDb specs
