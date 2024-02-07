-- | Generate asterix 'haskell' source code.

module Language.Haskell (mkCode) where

import           Control.Monad
import           Data.List
import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import           Formatting             as F

import           Asterix.Indent
import qualified Asterix.Specs          as A
import           Fmt
import           Struct

mkContent :: (Content, Int) -> BlockM Builder ()
mkContent (cont, ix) = case cont of
    ContentRaw -> do
        fmt ("type " % stext % " = 'TContentRaw") t
    ContentTable lst -> do
        let f :: (Int, Text) -> Text
            f (a,b) = sformat ("'( " % int % ", \"" % stext % "\")") a b
        fmt ("type " % stext % " = 'TContentTable " % stext) t (fmtList "'[ " "]" f lst)
    ContentString st -> do
        fmt ("type " % stext % " = 'TContentString " % stext) t $ case st of
            A.StringAscii -> "'StringAscii"
            A.StringICAO  -> "'StringICAO"
            A.StringOctal -> "'StringOctal"
    ContentInteger sig -> do
        fmt ("type " % stext % " = 'TContentInteger " % stext) t $ case sig of
            A.Signed   -> "'Signed"
            A.Unsigned -> "'Unsigned"
    ContentQuantity sig' lsb' unit -> do
        let sig = case sig' of
                A.Signed   -> "'Signed"
                A.Unsigned -> "'Unsigned"
            -- The following 'lsb*' functions is a pretty printer trick
            -- which follows the shape of the data structure. In this case,
            -- the parentheses are on the correct place automatically.
            lsb1 = \case
                A.NumInt i -> case i >= 0 of
                    True -> sformat ("'TNumInt 'TPlus " % int) i
                    False -> sformat ("'TNumInt 'TMinus " % int) (i * (-1))
                other -> lsb2 other
            lsb2 = \case
                A.NumDiv a b -> sformat ("'TNumDiv (" % stext % ") (" % stext % ")") (lsb1 a) (lsb1 b)
                other -> lsb3 other
            lsb3 = \case
                A.NumPow a b -> sformat ("'TNumPow " % int % " " % int) a b
                other -> lsb1 other
        fmt ("type " % stext % " = 'TContentQuantity " % stext
             % " (" % stext % ") \"" % stext % "\"") t sig (lsb1 lsb') unit
  where
    t = nameOf "TContent" ix

mkVariation :: AsterixDb EMap -> (Variation, Int) -> BlockM Builder ()
mkVariation db (var, ix) = case var of
    Element _ n cont -> do
        fmt ("type " % stext % " = 'TElement " % int % " " % stext) t n
            (nameOf "TContent" $ indexOf (dbContent db) cont)
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
    Explicit _ -> do
        fmt ("type " % stext % " = 'TExplicit") t
    Compound mn' lst -> do
        let mn = case mn' of
                Nothing -> "'Nothing"
                Just n  -> sformat ("('Just " % int % ")") (div8 n)
            f = \case
                Nothing -> "'Nothing"
                Just item -> "'Just " <> nameOf "TItem" (indexOf (dbItem db) item)
        fmt ("type " % stext % " = 'TCompound " % stext % " " % stext) t mn
            (fmtList "'[ " "]" f lst)
  where
    t = nameOf "TVariation" ix

mkItem :: AsterixDb EMap -> (Item, Int) -> BlockM Builder ()
mkItem db (item, ix) = case item of
    Spare _o n -> do
        fmt ("type " % stext % " = 'TSpare " % int) t n
    Item name title var -> do
        fmt ("type " % stext % " = 'TItem \"" % stext % "\" \"" % stext % "\" " % stext) t name title
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
mkCode :: Text -> Text -> [A.Asterix] -> Builder
mkCode ref ver specs' = render "    " "\n" $ do
    "-- | Asterix specifications" :: BlockM Builder ()
    ""
    "-- This file is generated, DO NOT EDIT!"
    "-- For more details, see:"
    "--    - https://github.com/zoranbosnjak/asterix-specs"
    ""
    "-- Types are BIG, disable depth checking."
    "{-# OPTIONS_GHC -freduction-depth=0 #-}"
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
