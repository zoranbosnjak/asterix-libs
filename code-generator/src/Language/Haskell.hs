-- | Generate asterix 'haskell' source code.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell (mkCode) where

import           Control.Monad
import           Data.List              (sort, nub)
import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import           Formatting             as F

import           Asterix.Indent
import qualified Asterix.Specs          as A
import           Fmt
import           Struct

-- The following functions is a pretty printer trick
-- which follows the shape of the data structure. In this case,
-- the parentheses are on the correct place automatically.
numberToText :: A.Number -> Text
numberToText = f1 where
    f1 = \case
        A.NumInt i -> case i >= 0 of
            True -> sformat ("'TNumInt 'TPlus " % int) i
            False -> sformat ("'TNumInt 'TMinus " % int) (i * (-1))
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
        fmt ("type " % stext % " = 'TContentRaw") t
    A.ContentTable lst -> do
        let f :: (Int, Text) -> Text
            f (a,b) = sformat ("'( " % int % ", \"" % stext % "\")") a b
        fmt ("type " % stext % " = 'TContentTable " % stext) t (fmtList "'[ " "]" f lst)
    A.ContentString st -> do
        fmt ("type " % stext % " = 'TContentString " % stext) t $ case st of
            A.StringAscii -> "'StringAscii"
            A.StringICAO  -> "'StringICAO"
            A.StringOctal -> "'StringOctal"
    A.ContentInteger sig cons -> do
        fmt ("type " % stext % " = 'TContentInteger " % stext % " " % stext) t
            ( case sig of
                 A.Signed   -> "'Signed"
                 A.Unsigned -> "'Unsigned")
            (fmtList "'[ " "]" constrainToText cons)
    A.ContentQuantity sig' lsb' unit cons -> do
        let sig = case sig' of
                A.Signed   -> "'Signed"
                A.Unsigned -> "'Unsigned"
        fmt ("type " % stext % " = 'TContentQuantity " % stext
             % " (" % stext % ") \"" % stext % "\" " % stext)
            t
            sig
            (numberToText lsb')
            unit
            (fmtList "'[ " "]" constrainToText cons)
    A.ContentBds bt -> do
        let x = case bt of
                A.BdsWithAddress -> "'TBdsWithAddress"
                A.BdsAt Nothing -> "('TBdsAt 'Nothing)"
                A.BdsAt (Just (A.BdsAddr addr)) -> sformat
                    ("('TBdsAt ('Just " % int % "))") addr
        fmt ("type " % stext % " = 'TContentBds " % stext) t x
  where
    t = nameOf "TContent" ix

mkRule :: AsterixDb EMap -> (A.Rule, Int) -> BlockM Builder ()
mkRule db (rule, ix) = case rule of
    A.ContextFree cont -> do
        fmt ("type " % stext % " = 'TContextFree " % stext)
            t
            (nameOf "TContent" $ indexOf (dbContent db) cont)
    A.Dependent name lst -> do
        let fText = sformat ("\"" % stext % "\"")
            fLst (i, cont) = sformat
                ("'(" % int % ", " % stext % ")")
                i
                (nameOf "TContent" $ indexOf (dbContent db) cont)
        fmt ("type " % stext % " = 'TDependent " % stext % " " % stext)
            t
            (fmtList "'[ " "]" fText name)
            (fmtList "'[ " "]" fLst lst)
  where
    t = nameOf "TRule" ix

mkVariation :: AsterixDb EMap -> (Variation, Int) -> BlockM Builder ()
mkVariation db (var, ix) = case var of
    Element (OctetOffset o) n rule -> do
        fmt ("type " % stext % " = 'TElement " % int % " " % int % " " % stext)
            t
            o
            n
            (nameOf "TRule" $ indexOf (dbRule db) rule)
    Group lst -> do
        let f :: Item -> Text
            f = nameOf "TItem" . indexOf (dbItem db)
        fmt ("type " % stext % " = 'TGroup " % stext) t
            (fmtList "'[ " "]" f lst)
    Extended lst -> do
        let f = \case
                Nothing -> "'Nothing"
                Just item -> "'Just " <> nameOf "TItem" (indexOf (dbItem db) item)
        fmt ("type " % stext % " = 'TExtended " % stext) t
            (fmtList "'[ " "]" f lst)
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
            (fmtList "'[ " "]" f lst)
  where
    t = nameOf "TVariation" ix

mkItem :: AsterixDb EMap -> (Item, Int) -> BlockM Builder ()
mkItem db (item, ix) = case item of
    Spare (OctetOffset o) n -> do
        fmt ("type " % stext % " = 'TSpare " % int % " " % int) t o n
    Item name title var -> do
        fmt ("type " % stext % " = 'TItem \"" % stext % "\" \"" %
             stext % "\" " % stext) t name title
            (nameOf "TVariation" $ indexOf (dbVariation db) var)
  where
    t = nameOf "TItem" ix

mkUap :: AsterixDb EMap -> (Uap, Int) -> BlockM Builder ()
mkUap db (uap, ix) = case uap of
    Uap var -> do
        fmt ("type " % stext % " = 'TUapSingle " % stext) t
            (nameOf "TVariation" $ indexOf (dbVariation db) var)
    Uaps lst _sel -> do
        let f (a, var) = sformat ("'(\"" % stext % "\", " % stext % ")") a
                (nameOf "TVariation" $ indexOf (dbVariation db) var)
        fmt ("type " % stext % " = 'TUapMultiple " % stext) t
            (fmtList "'[ " "]" f lst)
  where
    t = nameOf "TUap" ix

mkSpec :: AsterixDb EMap -> (AstSpec, Int) -> BlockM Builder ()
mkSpec db (spec, ix) = case spec of
    AstCat uap -> do
        fmt ("type " % stext % " = 'TCat " % stext) t
            (nameOf "TUap" $ indexOf (dbUap db) uap)
    AstRef var -> do
        fmt ("type " % stext % " = 'TRef " % stext) t
            (nameOf "TVariation" $ indexOf (dbVariation db) var)
  where
    t = nameOf "TSpec" ix

mkAsterix :: AsterixDb EMap -> (Asterix, Int) -> BlockM Builder ()
mkAsterix db ((Asterix cat (A.Edition a b) spec), ix) = do
    fmt ("type " % stext % " = 'TAsterix " % int % " " % stext % " " % stext)
        t
        cat
        (sformat ("('TEdition " % int % " " % int % ")") a b)
        (nameOf "TSpec" $ indexOf (dbSpec db) spec)
  where
    t = nameOf "TAsterix" ix

mkAlias :: (Asterix, Int) -> BlockM Builder ()
mkAlias (ast, ix) = do
    fmt ("type " % stext % " = " % stext)
        (nameOfAst ast)
        (nameOf "TAsterix" ix)

mkManifest :: [Asterix] -> BlockM Builder ()
mkManifest lst = do
    "manifest :: [VAsterix]"
    "manifest ="
    indent $ do
        forM_ (zip ps lst) $ \(p, ast) -> do
            fmt (stext % " schema @" % stext) p (nameOfAst ast)
        "]"
  where
    ps = ["["] <> repeat ","

-- | Source code generator entry point.
mkCode :: Bool -> Text -> Text -> [A.Asterix] -> Builder
mkCode test ref ver specs' = render "    " "\n" $ do
    "-- | Asterix specifications" :: BlockM Builder ()
    ""
    "-- This file is generated, DO NOT EDIT!"
    "-- For more details, see:"
    "--    - https://github.com/zoranbosnjak/asterix-specs"
    ""
    "{-# LANGUAGE OverloadedStrings #-}"
    "{-# LANGUAGE DataKinds #-}"
    ""
    "-- Types are BIG, disable depth checking."
    "{-# OPTIONS_GHC -freduction-depth=0 #-}"
    ""
    case test of
        True  -> "module Generated where"
        False -> "module Asterix.Generated where"
    ""
    "import           Data.Text"
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
    "-- | Rule set"
    sequence_ (fmap (mkRule db) $ enumList $ dbRule db)
    ""
    "-- | Variation set"
    sequence_ (fmap (mkVariation db) $ enumList $ dbVariation db)
    ""
    "-- | Item set"
    sequence_ (fmap (mkItem db) $ enumList $ dbItem db)
    ""
    "-- | Uap set"
    sequence_ (fmap (mkUap db) $ enumList $ dbUap db)
    ""
    "-- | Spec set"
    sequence_ (fmap (mkSpec db) $ enumList $ dbSpec db)
    ""
    "-- | Asterix set"
    sequence_ (fmap (mkAsterix db) $ enumList $ dbAst db)
    ""
    "-- | Aliases"
    sequence_ $ fmap mkAlias $ enumList $ dbAst db
    ""
    "-- | Manifest"
    mkManifest specs
    ""
  where
    specs :: [Asterix]
    specs = sort $ nub $ fmap deriveAsterix specs'

    db :: AsterixDb EMap
    db = enumDb $ asterixDb specs
