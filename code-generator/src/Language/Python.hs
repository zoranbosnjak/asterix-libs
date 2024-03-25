-- | Generate asterix 'python' source code.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: remove this
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Python where

import           Control.Monad
import           Data.List              (intersperse, nub, sort)
import           Data.Maybe
import           Data.Scientific
import           Data.Coerce
import           Data.Set               (Set)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import           Formatting             as F
import           Numeric                (showHex)

import           Asterix.Indent
import qualified Asterix.Specs          as A
import           Struct

fmt :: Format (BlockM Builder ()) a -> a
fmt m = runFormat m line

fmtList :: Text -> Text -> (a -> Text) -> [a] -> Text
fmtList open close f lst
    = open
   <> mconcat (intersperse ", " (fmap f lst))
   <> close

-- | We need database object in convert instance.
data Augmented t = Augmented (AsterixDb EMap) t

class Convert t where
    convert :: t -> BlockM Builder ()

instance Convert (Augmented (A.Content, Int)) where
    convert (Augmented _db (cont, ix)) = case cont of
        A.ContentRaw -> do
            clsArg "Raw"
            cls "ContentRaw"
            indent $ do
                "pass"
        A.ContentTable lst -> do
            let f :: (Int, Text) -> Text
                f (a,b) = sformat (int % ": \"" % stext % "\"") a b
            clsArg "Raw"
            cls "ContentTable"
            indent $ do
                fmt ("tab = " % stext) (fmtList "{" "}" f lst)
        A.ContentString st -> do
            clsArg "Union[Raw, str]"
            cls "ContentString"
            indent $ do
                fmt ("t = " % stext) $ case st of
                    A.StringAscii -> "StringAscii"
                    A.StringICAO  -> "StringICAO"
                    A.StringOctal -> "StringOctal"
        A.ContentInteger sig _cstr -> do
            clsArg "Raw"
            cls "ContentInteger"
            indent $ do
                fmt ("sig = " % stext) $ case sig of
                    A.Signed   -> "Signed"
                    A.Unsigned -> "Unsigned"
        A.ContentQuantity sig lsb unit _cstr -> do
            clsArg $ sformat ("Union[Raw, float, Tuple[float, Literal[\"" % stext % "\"]]]")
                (coerce unit)
            cls "ContentQuantity"
            indent $ do
                fmt ("sig = " % stext) $ case sig of
                    A.Signed   -> "Signed"
                    A.Unsigned -> "Unsigned"
                fmt ("lsb = " % scifmt Generic Nothing)
                    (fromFloatDigits (evalNumber lsb :: Double))
                fmt ("unit = \"" % stext % "\"") (coerce unit)
        A.ContentBds bt -> do
            clsArg "Raw"
            cls "ContentBds"
            indent $ case bt of
                A.BdsWithAddress -> do
                    "t = BdsWithAddress"
                A.BdsAt mAddr -> do
                    "t = BdsAt"
                    fmt ("addr = " % stext) $ case mAddr of
                        Nothing -> "None"
                        Just (A.BdsAddr addr) -> sformat int addr
      where
        clsArg = fmt ("Content_" % int % "_Arg : TypeAlias = " % stext) ix
        cls = fmt ("class Content_" % int % "(" % stext % "):") ix

instance Convert (Augmented (A.Rule A.Content, Int)) where
    convert (Augmented db (rule, ix)) = case rule of
        A.ContextFree cont -> do
            cls "RuleContentContextFree"
            indent $ do
                fmt ("content = Content_" % int) (indexOf (dbContent db) cont)
        A.Dependent items dv lst -> do
            cls "RuleContentDependent"
            indent $ do
                let f1 (A.ItemPath path) = fmtList "[" "]" f2 path
                    f2 (A.ItemName name) = "\"" <> name <> "\""
                fmt ("depends_on = " % stext)
                    (fmtList "[" "]" f1 items)
                fmt ("default_content = Content_" % int) (indexOf (dbContent db) dv)
                "cases = ["
                indent $ forM_ lst $ \(a, b) -> do
                    fmt ("(" % stext % ", Content_" % int % "),")
                        (fmtList "[" "]" (T.pack . show) a)
                        (indexOf (dbContent db) b)
                "]"
      where
        cls = fmt ("class RuleContent_" % int % "(" % stext % "):") ix

instance Convert (Augmented Variation) where
    convert (Augmented db var) = case var of
        A.Element o n rule -> do
            let ruleName = sformat ("RuleContent_" % int) (indexOf (dbRuleContent db) rule)
            -- fmt (stext % "_Arg : TypeAlias = " % stext % "_Arg") varName contName
            cls "Element"
            indent $ do
                fmt ("bit_offset8 = " % int) (coerce o :: Int)
                fmt ("bit_size = " % int) (coerce n :: Int)
                fmt ("rule = " % stext) ruleName
                {-
                ""
                fmt ("def __init__(self, arg : Union[Bits, " % stext % "_Arg]) -> None:") contName
                indent $ do
                    "if isinstance(arg, Bits):"
                    indent $ do
                        "super().__init__(arg)"
                        "return"
                    "n = self.__class__.bit_size"
                    fmt ("val = self.__class__.from_raw(" %
                        stext % ".from_arg(n, arg))") contName
                    "super().__init__(val)"
-}
        _ -> "# TODO"
      where
        ix = indexOf (dbVariation db) var
        cls = fmt ("class Variation_" % int % "(" % stext % "):") ix
        {-
        Group lst -> do
            let varName2 var2 = nameOf "Variation" $ indexOf (dbVariation db) var2
                items = catMaybes $ flip fmap lst $ \case
                    Spare _ _ -> Nothing
                    Item name title var2 -> Just (name, title, var2)
            fmt (stext % "_Arg_Group = TypedDict('" % stext % "_Arg_Group', {") varName varName
            indent $ forM_ items $ \(name, _title, rule2) -> do
                let var2 = unRule rule2
                fmt ("\"" % stext % "\": Union[" % stext % ", " % stext % "_Arg],")
                    name (varName2 var2) (varName2 var2)
            "})"
            fmt (stext % "_Arg : TypeAlias = Union[Raw, "% stext % "_Arg_Group]")
                varName varName
            cls "Group"
            indent $ do
                case sizeOfVariation var of
                    Nothing -> error "unexpected non-fixed argument"
                    Just n  -> fmt ("bit_size = " % int) n
                do -- items_list
                    let f :: Item -> Text
                        f = nameOf "Item" . indexOf (dbItem db)
                    fmt ("items_list = " % stext)
                        (fmtList "[" "]" f lst)
                do -- items_dict
                    let f (name, title, var2) = sformat
                            ("\"" % stext % "\": (" % stext % ")")
                            name
                            (nameOf "Item" $ indexOf (dbItem db) (Item name title var2))
                    fmt ("items_dict = " % stext)
                        (fmtList "{" "}" f items)
        Extended lst -> do
            cls "Extended"
            indent $ do
                let f :: Maybe Item -> Text
                    f Nothing     = "None"
                    f (Just item) = nameOf "Item" $ indexOf (dbItem db) item
                fmt ("items = " % stext)
                    (fmtList "[" "]" f lst)
        Repetitive rt var2 -> do
            cls "Repetitive"
            indent $ do
                case rt of
                    A.RepetitiveRegular n -> fmt ("rep = " % int) (div8 n)
                    A.RepetitiveFx        -> "rep = None"
                fmt ("var = " % stext)
                    (nameOf "Variation" $ indexOf (dbVariation db) var2)
        Explicit et -> do
            cls "Explicit"
            indent $ do
                fmt ("t = " % stext) $ case et of
                    Nothing                  -> "None"
                    Just A.ReservedExpansion -> "ReservedExpansion"
                    Just A.SpecialPurpose    -> "SpecialPurpose"
        Compound mn lst' -> do
            let lst = fmap simplifyCompoundSubitem lst'
            cls "Compound"
            indent $ do
                case mn of
                    Nothing -> "fspec_size = None"
                    Just n  -> fmt ("fspec_size = " % int) (div8 n)
                do -- items_list
                    let f :: Maybe Item -> Text
                        f Nothing     = "None"
                        f (Just item) = nameOf "Item" $ indexOf (dbItem db) item
                    fmt ("items_list = " % stext)
                        (fmtList "[" "]" f lst)
                do -- items_dict
                    let showFspec :: Fspec -> Text
                        showFspec fspecs =
                            let f = T.pack . reverse . take 2 . (<> "00") . reverse . flip showHex ""
                            in mconcat $ fmap f fspecs
                    let f2 = \case
                            Just (Item name title var2) -> Just (name, title, var2)
                            _ -> Nothing
                        f1 (name, title, var2) = sformat
                            ("\"" % stext % "\": (" % stext % ", 0x" % stext % ")")
                            name
                            (nameOf "Item" $ indexOf (dbItem db) (Item name title var2))
                            (showFspec $ fspecOf mn lst name)
                    fmt ("items_dict = " % stext)
                        (fmtList "{" "}" f1 $ mapMaybe f2 lst)
                do -- spec
                    let relevant = flip mapMaybe lst $ \case
                            Just (Item name _title rule2) -> Just (name, unRule rule2)
                            _ -> Nothing
                    case relevant of
                        [] -> pure ()
                        [(name, var2)] -> do
                            "@classmethod"
                            fmt ("def spec(cls, key : Literal[\"" % stext % "\"]) -> " % stext % ":")
                                name
                                (nameOf "Variation" $ indexOf (dbVariation db) var2)
                            indent $ do
                                "return cls.items_dict[key][1] # type: ignore"
                        _ -> do
                            forM_ relevant $ \(name, var2) -> do
                                "@overload"
                                "@classmethod"
                                fmt ("def spec(cls, key : Literal[\"" %
                                    stext % "\"]) -> " % stext % ": ...")
                                    name
                                    (nameOf "Variation" $ indexOf (dbVariation db) var2)
                            "@classmethod"
                            "def spec(cls, key : Any) -> Any:"
                            indent $ do
                                "return cls.items_dict[key][1]"

    where
        ix = indexOf (dbVariation db) var
        varName = nameOf "Variation" ix
        cls c = fmt ("class " % stext % "(" % stext % "):") varName c
-}

instance Convert (Augmented NonSpare) where
    convert (Augmented db nsp) = do
        cls "NonSpare"
        indent $ do
            "pass # TODO"
      where
        ix = indexOf (dbNonSpare db) nsp
        cls = fmt ("class NonSpare_" % int % "(" % stext % "):") ix

instance Convert (Augmented Item) where
    convert (Augmented db item) = do
        cls ""
        indent $ do
            "pass # TODO"
      where
        ix = indexOf (dbItem db) item
        cls = fmt ("class Item_" % int % "(" % stext % "):") ix


{-
mkItem :: AsterixDb EMap -> Item -> BlockM Builder ()
mkItem db item = case item of
    Spare (OctetOffset o) n -> do
        cls "Spare"
        indent $ do
            fmt ("bit_offset8 = " % int) o
            fmt ("bit_size = " % int) n
    Item name title rule -> do
        cls "Item"
        indent $ do
            fmt ("name = \"" % stext % "\"") name
            fmt ("title = \"" % stext % "\"") title
            fmt ("var = " % stext)
                (nameOf "Variation" $ indexOf (dbVariation db) (unRule rule))
    where
        ix = indexOf (dbItem db) item
        cls c = fmt ("class " % stext % "(" % stext % "):") (nameOf "Item" ix) c

mkRecord :: AsterixDb EMap -> (Record, Int) -> BlockM Builder ()
mkRecord db (Record lst', ix) = do
    let lst = fmap simplifyCompoundSubitem lst'
    cls "Record"
    indent $ do
        let f :: Maybe Item -> Text
            f Nothing     = "None"
            f (Just item) = nameOf "Item" $ indexOf (dbItem db) item
        fmt ("items = " % stext)
            (fmtList "[" "]" f lst)
  where
    cls c = fmt ("class " % stext % "(" % stext % "):") (nameOf "Record" ix) c

mkExpansion :: AsterixDb EMap -> (Expansion, Int) -> BlockM Builder ()
mkExpansion db (Expansion n lst', ix) = do
    let lst = fmap simplifyCompoundSubitem lst'
    cls "Expansion"
    indent $ do
        fmt ("fspec_bytes = " % int) (div8 n)
        let f :: Maybe Item -> Text
            f Nothing     = "None"
            f (Just item) = nameOf "Item" $ indexOf (dbItem db) item
        fmt ("items = " % stext)
            (fmtList "[" "]" f lst)
  where
    cls c = fmt ("class " % stext % "(" % stext % "):") (nameOf "Expansion" ix) c

mkUap :: AsterixDb EMap -> (Uap, Int) -> BlockM Builder ()
mkUap db (uap, ix) = case uap of
    Uap record -> do
        cls "UapSingle"
        indent $ do
            fmt ("record = " % stext)
                (nameOf "Record" $ indexOf (dbRecord db) record)
    Uaps lst msel -> do
        cls "UapMultiple"
        indent $ do
            let f :: (A.UapName, Record) -> Text
                f (name, record) = sformat ("\"" % stext % "\": " % stext)
                    name
                    (nameOf "Record" $ indexOf (dbRecord db) record)
            fmt ("uaps = " % stext)
                (fmtList "{" "}" f lst)
            case msel of
                Nothing -> "selector = None"
                Just (A.UapSelector item tab) -> do
                    let f1 :: A.Name -> Text
                        f1 = sformat ("\"" % stext % "\"")
                        f2 :: (Int, A.UapName) -> Text
                        f2 (val, name) = sformat
                            (int % ": \"" % stext % "\"") val name
                    fmt ("selector = (" % stext % ", " % stext % ")")
                       (fmtList "[" "]" f1 item)
                       (fmtList "{" "}" f2 tab)
  where
    cls c = fmt ("class " % stext % "(" % stext % "):")
        (nameOf "Uap" ix) c

mkAstSpec :: AsterixDb EMap -> (AstSpec, Int) -> BlockM Builder ()
mkAstSpec db (astSpec, ix) = case astSpec of
    AstCat (Cat cat) (A.Edition a b) uap -> do
        cls "AstCat"
        indent $ do
            fmt ("cat = " % int) cat
            fmt ("edition = (" % int % ", " % int % ")") a b
            fmt ("uap = " % stext)
                (nameOf "Uap" $ indexOf (dbUap db) uap)
    AstRef (Cat cat) (A.Edition a b) expan -> do
        cls "AstRef"
        indent $ do
            fmt ("cat = " % int) cat
            fmt ("edition = (" % int % ", " % int % ")") a b
            fmt ("expansion = " % stext)
                (nameOf "Expansion" $ indexOf (dbExpansion db) expan)
  where
    cls c = fmt ("class " % stext % "(" % stext % "):")
        (nameOf "AstSpec" ix) c

mkAlias :: (AstSpec, Int) -> BlockM Builder ()
mkAlias (astSpec, ix) = do
    fmt (stext % ": TypeAlias = " % stext)
        (nameOfAst astSpec)
        (nameOf "AstSpec" ix)

mkManifest :: [AstSpec] -> BlockM Builder ()
mkManifest lst = do
    let f :: AstSpec -> Text
        f ast = sformat stext (nameOfAst ast)
    fmt ("manifest = " % stext) (fmtList "[" "]" f lst)

{-
-- | Name of argument with given (variation) index.
argOf :: VariationIx -> Text
argOf vc = nameOf vc <> "_Arg"

-- | Python building blocks

typeAlias :: Text -> Text -> BlockM Builder ()
typeAlias a b = fmt (stext % " : TypeAlias = " % stext) a b

tList :: (Monoid t, IsString t) => [t] -> t
tList = mconcat . intersperse ", "

pyClass :: Text -> [Text] -> BlockM Builder () -> BlockM Builder ()
pyClass name bases body = do
    fmt ("class " % stext % stext % ":") name blist
    indent body
 where
    blist = case bases of
        [] -> ""
        _ -> "(" <> tList bases <> ")"

pyFunc :: Text -> [Text] -> Text -> BlockM Builder () -> BlockM Builder ()
pyFunc name args ret body = do
    fmt ("def " % stext % "(" % stext % ") -> " % stext % ":") name (tList args) ret
    indent body

pyIf :: Text -> BlockM Builder () -> BlockM Builder ()
pyIf cond body = do
    fmt ("if " % stext % ":") cond
    indent body

pyList :: Text -> [BlockM Builder ()] -> BlockM Builder ()
pyList name lst = enclose (line $ BL.fromText $ name <> " = [") "]" (mconcat lst)

pyDict :: Text -> [BlockM Builder ()] -> BlockM Builder ()
pyDict name lst = enclose (line $ BL.fromText $ name <> " = {") "}" (mconcat lst)

subSpecs :: [(Name, Text)] -> BlockM Builder ()
subSpecs lst = blocksLn
    [ bool (pure ()) (blocksLn $ fmap overload lst) (length lst > 1)
    , specsFunc
    ]
  where
    overload (name, cls) = do
        line "@overload"
        line "@classmethod"
        pyFunc "spec"
            ["cls", "key : Literal[" <> escaped name <> "]"]
            ("Type[" <> cls <> "]")
            "..."

    a = mconcat $ intersperse ", " $ do
        (name, _x) <- lst
        pure $ "Literal[" <> escaped name <> "]"

    b = mconcat $ intersperse ", " $ do
        (_name, cls) <- lst
        pure $ "Type['" <> cls <> "']"

    specsFunc = do
        line "@classmethod"
        pyFunc "spec"
            ["cls", "key : Union[" <> a <> "]"]
            ( "Union[" <> b <> "]") $ do
                forM_ lst $ \(name, cls) -> do
                    pyIf ("key == " <> escaped name)
                        (line $ "return " <> BL.fromText cls)
                line $ "assert_never(key)"

handleElement :: VariationIx -> OctetOffset -> RegisterSize -> Content -> BlockM Builder ()
handleElement vc o n cont = do
    typeAlias (argOf vc) arg
    pyClass (nameOf vc) ["Element"] $ blocksLn
        [ fmt "variation = 'Element'"
        , constants
        , initFunc
        , tableLookup
        , toString
        , toQuantity
        ]
  where
    arg = case cont of
        ContentString _st -> "Union[Raw,str]"
        ContentQuantity _sig _lsb unit ->
            "Union[Raw,float,Tuple[float,Literal[" <> escaped unit <> "]]]"
        _ -> "Raw"

    tableConst = case cont of
        ContentTable lst -> enclose "table = {" "}" $ mconcat $ do
            (x, t) <- lst
            pure $ fmt (int % ": " % stext % ",") x (escaped t)
        _ -> pure ()

    strConst = case cont of
        ContentString st -> fmt ("string_type = " % string % "()") (show st)
        _ -> pure ()

    realNum :: Number -> Double
    realNum = \case
        NumInt i -> fromIntegral i
        NumDiv a b -> realNum a / realNum b
        NumPow a b -> fromIntegral (a ^ b)

    quantityConst = case cont of
        ContentQuantity sig lsb unit -> fmt ("quantity = Quantity("
            % "'" % string % "'"
            % ", " % string
            % ", " % stext
            % ")") (show sig) (show $ realNum lsb) (escaped unit)
        _ -> pure ()

    constants = do
        fmt ("bit_offset8 = " % int) (unOctetOffset o)
        fmt ("bit_size = " % int) n
        tableConst
        strConst
        quantityConst

    initFunc = pyFunc "__init__" ["self", "arg : " <> argOf vc] "None" $ do
        pyIf "isinstance(arg, Bits)"
            "super().__init__(arg); return"
        pyIf "isinstance(arg, Raw)"
            "super().__init__(self._from_raw(arg)); return"
        case cont of
            ContentString _st -> pyIf "isinstance(arg, str)"
                "super().__init__(self._from_string(arg)); return"
            ContentQuantity _sig _lsb _unit -> do
                pyIf "isinstance(arg, float)" "super().__init__(self._from_float(arg)); return"
                pyIf "isinstance(arg, tuple)" "super().__init__(self._from_float(arg[0])); return"
            _ -> pure ()
        line $ "assert_never(arg)"

    tableLookup = case cont of
        ContentTable _lst -> do
            line $ "@property"
            pyFunc "table_value" ["self"] "Optional[str]" $
                "return self.__class__.table.get(self.to_uinteger())"
        _ -> pure ()

    toString = case cont of
        ContentString _st -> pyFunc "to_string" ["self"] "str" $
            "return self._to_string()"
        _ -> pure ()

    toQuantity = case cont of
        ContentQuantity _sig _lsb _unit -> pyFunc "to_quantity" ["self"] "float" $
            "return self._to_quantity()"
        _ -> pure ()

-- | Filter items, keep only non-spare items.
nonSpare :: [(a, b, Item)] -> [(a, b, Name, Title, Variation)]
nonSpare lst = do
    (goff, n, item) <- lst
    case item of
        Spare _o _n -> empty
        Item name title var -> pure (goff, n, name, title, var)

handleGroup :: VariationDb -> VariationIx -> [(GroupOffset, RegisterSize, Item)] -> BlockM Builder ()
handleGroup db vc lst = do
    argGroup
    typeAlias (argOf vc) ("Union[Raw, " <> argOf vc <> "_Group]")
    pyClass (nameOf vc) ["Group"] $ blocksLn
        [ fmt "variation = 'Group'"
        , bitSize
        , subitemsList
        , subitemsDict
        , spec
        , initFunc
        , getItem
        , setItem
        , modifyItem
        ]
  where
    nLst = do
        (a, b, c, d, var) <- nonSpare lst
        pure (a, b, c, d, indexOf db var)

    argGroup = do
        let a = argOf vc <> "_Group = TypedDict('" <> argOf vc <> "_Group', {"
            b = "})"
        enclose (fmt stext a) b $ mconcat $ do
            (_goff, _n, name, _title, ix) <- nLst
            pure $ fmt (stext % ": Union[" % stext % ", " % stext % "],")
                (escaped name)
                (nameOf ix)
                (argOf ix)

    bitSize = fmt ("bit_size = " % int) $ sum [n | (_,n,_) <- lst]

    subitemsList = pyList "subitems_list" $ do
        (_goff, _n, item) <- lst
        pure $ case item of
            Spare o n -> fmt ("Spare(" % int % ", " % int % "),") (unOctetOffset o) n
            Item name _title var -> fmt ("(" % stext % ", " % stext % "),")
                (escaped name) (nameOf $ indexOf db var)

    subitemsDict = do
        line "# name: (title, cls, group_offset, bit_size)"
        pyDict "subitems_dict" $ do
            (goff, n, name, title, ix) <- nLst
            pure $ fmt (stext % ": (" % stext % ", " % stext % ", " % int % ", " % int % "),")
                (escaped name)
                (escaped title)
                (nameOf ix)
                goff
                n

    spec = subSpecs $ do
        (_goff, _n, name, _title, ix) <- nLst
        pure (name, nameOf ix)

    initFunc = pyFunc "__init__" ["self", "arg : " <> argOf vc] "None" $ do
        pyIf "isinstance(arg, tuple)" "super().__init__(*arg); return"
        pyIf "isinstance(arg, dict)" "super().__init__(*self._from_items(arg)); return"
        pyIf "isinstance(arg, Raw)" "super().__init__(*self._from_raw(arg)); return"
        "assert_never(arg)"

    getItem = case length nLst of
        1 -> pyFunc "get_item" ["self", "name : " <> a ] "Any"
                "return self._get_item(name)"
          where
            (_goff, _n, name, _title, _cls) = head nLst
            a = "Literal[" <> escaped name <> "]"

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, ix) <- nLst
                pure $ do
                    line "@overload"
                    pyFunc "get_item" ["self", "name : Literal[" <> escaped name <> "]"]
                        (nameOf ix)
                        "..."
            , pyFunc "get_item" ["self", "name : Union[" <> a <> "]"] "Any"
                "return self._get_item(name)"
            ]
          where
            a = mconcat $ intersperse ", " $ do
                (_goff, _n, name, _title, _ix) <- nLst
                pure $ "Literal[" <> escaped name <> "]"

    setItem = case length nLst of
        1 -> pyFunc "set_item" ["self", "name : Literal[" <> escaped name <> "]", "val : " <> arg]
            ("'" <> nameOf vc <> "'")
            "return self._set_item(name, val) # type: ignore"
          where
            (_goff, _n, name, _title, ix) = head nLst
            arg = "Union[" <> nameOf ix <> ", " <> argOf ix <> "]"

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, ix) <- nLst
                let arg = "Union[" <> nameOf ix <> ", " <> argOf ix <> "]"
                pure $ do
                    line "@overload"
                    pyFunc "set_item" ["self", "name : Literal[" <> escaped name <> "]", "val : " <> arg]
                        ("'" <> nameOf vc <> "'")
                        "..."
            , pyFunc "set_item" ["self", "name : Any", "val : Any"] "Any"
                "return self._set_item(name, val)"
            ]

    modifyItem = case length nLst of
        1 -> pyFunc "modify_item" ["self", "name : Literal[" <> escaped name <> "]", "f : Any"]
            ("'" <> nameOf vc <> "'")
            "return self._modify_item(name, f) # type: ignore"
          where
            (_goff, _n, name, _title, _ix) = head nLst

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, _ix) <- nLst
                pure $ do
                    line "@overload"
                    pyFunc "modify_item" ["self", "name : Literal[" <> escaped name <> "]", "f : Any"]
                        ("'" <> nameOf vc <> "'")
                        "..."
            , pyFunc "modify_item" ["self", "name : Any", "f : Any"] "Any"
                "return self._modify_item(name, f)"
            ]

handleExtended :: VariationDb -> VariationIx
    -> [GroupMember] -> [[GroupMember]] -> TrailingFx
    -> BlockM Builder ()
handleExtended db vc prim ext trfx = do
    mconcat (fmap mkGrp grps')
    argUnion
    pyClass (nameOf vc) ["Extended"] $ blocksLn
        [ fmt "variation = 'Extended'"
        , constants
        , pyList "subitems_list" (fmap subitemsList grps)
        , subitemsDict
        , spec
        , initFunc
        , getItem
        , setItem
        , modifyItem
        ]
  where
    grps = prim : ext

    grps' :: [(Int, [GroupMember])]
    grps' = zip [1..] (fmap mconcat (drop 1 $ inits grps))

    ag :: Int -> Text
    ag n = argOf vc <> "_Group_" <> sformat int n

    mkGrp :: (Int, [(GroupOffset, RegisterSize, Item)]) -> BlockM Builder ()
    mkGrp (n, lst) = enclose a b $ mconcat $ do
        (_goff, _n, name, _title, var) <- nonSpare lst
        let cls = nameOf $ indexOf db var
        pure $ fmt (stext % ": Union[" % stext % ", " % stext % "_Arg],")
            (escaped name) cls cls
      where
        a = fmt (stext % " = TypedDict('" % stext % "', {") (ag n) (ag n)
        b = "})"

    argUnion = enclose a "]" $ mconcat ("int," : args)
      where
        a = fmt (stext % " : TypeAlias = Union[") (argOf vc)
        args = do
            (n, _lst) <- grps'
            pure $ do
                fmt (stext % ",") (ag n)
                line $ "Tuple["
                    <> mconcat (intersperse "," (replicate n "int"))
                    <> "],"

    constants = do
        line $ "no_trailing_fx = " <> bool "True" "False" trfx
        enclose "groups_bit_sizes = [" "]" $ mconcat $ do
            lst <- grps
            pure $ fmt (int % ",") (sum [n | (_,n,_) <- lst])

    subitemsList lst = enclose "[" "]," $ mconcat $ do
        (_goff, _n, item) <- lst
        pure $ case item of
            Spare o n -> fmt ("Spare(" % int % ", " % int % "),") (unOctetOffset o) n
            Item name _title var -> fmt
                ("(" % stext % ", " % stext % "),")
                (escaped name)
                (nameOf $ indexOf db var)

    subitemsDict = do
        line "# name: (title, cls, group_offset, bit_size)"
        pyDict "subitems_dict" $ do
            (goff, n, name, title, var) <- nonSpare (join grps)
            let cls = nameOf $ indexOf db var
            pure $ fmt (stext % ": (" % stext % ", " % stext % ", " % int % ", " % int % "),")
                (escaped name) (escaped title) cls goff n

    spec = subSpecs $ do
        (_goff, _n, name, _title, var) <- nonSpare (join grps)
        let cls = nameOf $ indexOf db var
        pure (name, cls)

    initFunc = pyFunc "__init__" ["self", "arg : " <> argOf vc] "None" $ do
        -- In the case of single group, the fx is present.
        pyIf "isinstance(arg, int)"
            "super().__init__(*self._from_single_int(0, arg, False)); return"
        pyIf "isinstance(arg, tuple)" $ do
            pyIf "isinstance(arg[0], Bits)"
                "super().__init__(*arg); return"
            "super().__init__(*self._from_tuple_int(arg)); return"
        pyIf "isinstance(arg, dict)" $ do
            forM_ grps' $ \(n, _lst) -> do
                pyIf (ag n <> ".__required_keys__ == arg.keys()") $
                    fmt ("super().__init__(*self._from_dict(" % int % ", arg)); return") n
            "raise Exception('internal error: unexpected argument')"
        "assert_never(arg)"

    getItem = case length lst of
        1 -> pyFunc "get_item" ["self", "name : " <> a ] "Any"
                "return self._get_item(name)"
          where
            (_goff, _n, name, _title, _cls) = head lst
            a = "Literal[" <> escaped name <> "]"

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, var) <- lst
                let cls = nameOf $ indexOf db var
                pure $ mconcat
                    [ "@overload"
                    , pyFunc "get_item" ["self", "name : Literal[" <> escaped name <> "]"]
                        cls
                        "..."
                    ]
            , pyFunc "get_item" ["self", "name : Union[" <> a <> "]"] "Any"
                "return self._get_item(name)"
            ]
          where
            a = mconcat $ intersperse ", " $ do
                (_goff, _n, name, _title, _cls) <- lst
                pure $ "Literal[" <> escaped name <> "]"
      where
        lst = nonSpare (join grps)

    setItem = case length lst of
        1 -> pyFunc "set_item" ["self", "name : Literal[" <> escaped name <> "]", "val : " <> arg]
            ("'" <> nameOf vc <> "'")
            "return self._set_item(name, val) # type: ignore"
          where
            (_goff, _n, name, _title, var) = head lst
            ix = indexOf db var
            arg = "Union[" <> nameOf ix <> ", " <> argOf ix <> "]"

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, var) <- lst
                let ix = indexOf db var
                    arg = "Union[" <> nameOf ix <> ", " <> argOf ix <> "]"
                pure $ do
                    line "@overload"
                    pyFunc "set_item" ["self", "name : Literal[" <> escaped name <> "]", "val : " <> arg]
                        ("'" <> nameOf vc <> "'")
                        "..."
            , pyFunc "set_item" ["self", "name : Any", "val : Any"] "Any"
                "return self._set_item(name, val)"
            ]
      where
        lst = nonSpare (head grps) -- only for primary part

    modifyItem = case length lst of
        1 -> pyFunc "modify_item" ["self", "name : Literal[" <> escaped name <> "]", "f : Any"]
            ("'" <> nameOf vc <> "'")
            "return self._modify_item(name, f) # type: ignore"
          where
            (_goff, _n, name, _title, _ix) = head lst

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, _ix) <- lst
                pure $ do
                    line "@overload"
                    pyFunc "modify_item" ["self", "name : Literal[" <> escaped name <> "]", "f : Any"]
                        ("'" <> nameOf vc <> "'")
                        "..."
            , pyFunc "modify_item" ["self", "name : Any", "f : Any"] "Any"
                "return self._modify_item(name, f)"
            ]
      where
        lst = nonSpare (join grps)

handleRepetitive :: VariationDb -> VariationIx -> RepetitiveType -> RegisterSize -> Variation -> BlockM Builder ()
handleRepetitive db vc rt varBitSize var = do
    typeAlias (argOf vc) arg
    pyClass (nameOf vc) ["Repetitive"] $ blocksLn
        [ fmt "variation = 'Repetitive'"
        , constants
        , spec
        , initFunc
        , xPendItem "append_item"
        , xPendItem "prepend_item"
        ]
  where
    iv = indexOf db var
    arg = "Union[" <> nameOf iv <> ", " <> argOf iv <> "]"
    argList = "List[" <> arg <> "]"

    cnV = nameOf $ indexOf db var

    constants = do
        case rt of
            RepetitiveRegular repBitSize -> do
                let (repByteSize, b) = divMod repBitSize 8
                assert "repetition size" (b==0)
                fmt ("rep_byte_size = " % int) repByteSize
            RepetitiveFx -> do
                fmt "rep_byte_size = None"
        fmt ("variation_bit_size = " % int) varBitSize
        fmt ("variation_type = " % stext) cnV

    spec = do
        line "@classmethod"
        pyFunc "spec" ["cls"] ("Type[" <> cnV <> "]") $ do
            fmt ("return " % stext) cnV

    initFunc = pyFunc "__init__" ["self", "arg : " <> argList] "None" $ do
        pyIf "isinstance(arg, tuple)" "super().__init__(*arg); return"
        pyIf "isinstance(arg, list)" "super().__init__(*self._from_list(arg)); return"
        "assert_never(arg)"

    xPendItem f = pyFunc f ["self", "arg : " <> arg] ("'" <> nameOf vc <> "'") $ do
        fmt ("return self._" % stext % "(arg) # type: ignore") f

handleExplicit :: VariationIx -> Maybe ExplicitType -> BlockM Builder ()
handleExplicit vc et = do
    typeAlias (argOf vc) "bytes"
    pyClass (nameOf vc) ["Explicit"] $ blocksLn
        [ fmt "variation = 'Explicit'"
        , constants
        , initFunc
        ]
  where
    constants = do
        fmt ("explicit_type = " % stext) (maybe "None" (T.pack . show . show) et)
    initFunc = pyFunc "__init__" ["self", "arg : bytes"] "None" $ mconcat
        [ pyIf "isinstance(arg, tuple)" "super().__init__(*arg); return"
        , pyIf "isinstance(arg, bytes)" "super().__init__(*self._from_bytes(arg)); return"
        , "assert_never(arg)"
        ]

handleCompound :: VariationDb -> VariationIx -> Maybe ByteSize -> ByteSize
    -> [Maybe (Name, Title, Variation, Fspec)]
    -> BlockM Builder ()
handleCompound db vc mn fspec_max_bytes lst = do
    hdr
    pyClass (nameOf vc) ["Compound"] $ blocksLn
        [ fmt "variation = 'Compound'"
        , constants
        , subitemsList
        , subitemsDict
        , spec
        , initFunc
        , setItem
        , delItem
        , getItem
        , modifyItem
        ]
  where
    hdr = enclose a b $ mconcat $ do
        (name, _title, var, _fspec) <- catMaybes lst
        let iv = indexOf db var
        pure $ fmt (stext % ": Union[" % stext % ", " % stext % "],")
            (escaped name)
            (nameOf iv)
            (argOf iv)
      where
        a = fmt (stext % " = TypedDict('" % stext % "', {") (argOf vc) (argOf vc)
        b = "}, total=False)"

    constants = do
        fmt ("fspec_fx = " % stext) (maybe "True" (const "False") mn)
        fmt ("fspec_max_bytes = " % int) fspec_max_bytes

    subitemsList = enclose "subitems_list = [" "]" $ mconcat $ do
        mItem <- lst
        pure $ case mItem of
            Nothing -> "None,"
            Just (name, _title, var, _fspec) ->
                fmt ("(" % stext % ", " % stext % "),")
                    (escaped name)
                    (nameOf $ indexOf db var)

    showFspec :: Fspec -> Text
    showFspec fspecs =
        let f = T.pack . reverse . take 2 . (<> "00") . reverse . flip showHex ""
        in mconcat $ fmap f fspecs

    subitemsDict = do
        line "# name: (title, cls, fspec)"
        enclose "subitems_dict = {" "}" $ mconcat $ do
            (name, title, var, fspec) <- catMaybes lst
            pure $ fmt (stext % ": (" % stext % ", " % stext % ", 0x" % stext % "),")
                (escaped name)
                (escaped title)
                (nameOf $ indexOf db var)
                (showFspec fspec)

    spec = subSpecs $ do
        (name, _title, var, _fspec) <- catMaybes lst
        pure (name, nameOf $ indexOf db var)

    initFunc = pyFunc "__init__" ["self", "arg : Optional[" <> argOf vc <> "] = None"] "None" $ do
        pyIf "isinstance(arg, tuple)"
            "super().__init__(*arg); return"
        line "super().__init__()"
        line "if arg is not None: self._update(arg)"

    setItem = blocksLn
        [ blocksLn $ do
            (name, _title, var, _fspec) <- catMaybes lst
            let iv = indexOf db var
                arg = "Union[" <> (nameOf iv) <> ", " <> argOf iv <> "]"
            pure $ do
                line "@overload"
                pyFunc "set_item"
                    [ "self"
                    , "name : Literal[" <> escaped name <> "]"
                    , "val : " <> arg
                    ]
                    ("'" <> nameOf vc <> "'")
                    "..."
        , pyFunc "set_item" ["self", "name : Any, val : Any"] "Any"
            "return self._set_item(name, val)"
        ]

    delItem = blocksLn
        [ blocksLn $ do
            (name, _title, _var, _fspec) <- catMaybes lst
            pure $ do
                line "@overload"
                pyFunc "del_item" ["self", "name : Literal[" <> escaped name <> "]"]
                    ("'" <> nameOf vc <> "'")
                    "..."
        , pyFunc "del_item" ["self", "name : Any"] "Any"
            "return self._del_item(name)"
        ]

    getItem = blocksLn
        [ blocksLn $ do
            (name, _title, var, _fspec) <- catMaybes lst
            pure $ mconcat
                [ "@overload"
                , pyFunc "get_item" ["self", "name : Literal[" <> escaped name <> "]"]
                    (nameOf $ indexOf db var)
                    "..."
                ]
        , pyFunc "get_item" ["self", "name : Any"] "Any"
            "return self._get_item(name)"
        ]

    modifyItem = blocksLn
        [ blocksLn $ do
            (name, _title, _var, _fspec) <- catMaybes lst
            pure $ do
                line "@overload"
                pyFunc "modify_item" ["self", "name : Literal[" <> escaped name <> "]", "f : Any"]
                    ("'" <> nameOf vc <> "'")
                    "..."
        , pyFunc "modify_item" ["self", "name : Any", "f : Any"] "Any"
            "return self._modify_item(name, f)"
        ]

-- | Create 'Block', representing a 'Variation', call proper handlers.
variationBlock :: VariationDb -> VariationIx -> Variation -> BlockM Builder ()
variationBlock db vc variation = case variation of
    Element o n cont -> handleElement vc o n cont
    Group lst -> handleGroup db vc lst
    Extended prim ext trfx -> handleExtended db vc prim ext trfx
    Repetitive rt varBitSize var2 -> handleRepetitive db vc rt varBitSize var2
    Explicit et -> handleExplicit vc et
    Compound mn fspec_max_bytes lst -> handleCompound db vc mn fspec_max_bytes lst

-- | Create top-level variations.
programVariations :: Bool -> VariationDb -> BlockM Builder ()
programVariations includeComments db = blocksLn $ do
    (n, paths, var) <- lst
    pure $ do
        when includeComments $ forM_ (Set.toList paths) $ \path -> do
            line $ BL.fromText $ "# " <> tPath path
        variationBlock db n var
  where
    lst :: [(VariationIx, Set Path, Variation)]
    lst
        = sortOn (\(n,_,_) -> n)
        $ flip Map.foldMapWithKey db $ \var (n, paths) -> [(n, paths, var)]

-- | Create top-level asterix spec.
handleSpec :: VariationDb -> Asterix -> BlockM Builder ()
handleSpec db ast = case astSpec ast of
    AstCat uap -> pyClass (specName ast) ["Basic"] $ case uap of
        Uap var -> blocksLn
            [ catLine >> varLine (nameOf $ iv var)
            , specLine
            , parseBits, unparseBits
            , fMkRecord (iv var)
            , fMkDatablock (nameOf $ iv var)
            , fParseSingle (nameOf $ iv var)
            ]
        Uaps uaps msel -> blocksLn
            [ catLine
            , fUap uaps msel
            , fMkRecordUnsafe uaps
            , fMkDatablock $ "Union[" <> tList [nameOf (iv var) | (_name, var) <- uaps] <> "]"
            , fParseMultiple uaps msel
            , maybe (pure ()) (fIsValid uaps) msel
            ]
    AstRef var -> pyClass (specName ast) ["Expansion"] $ blocksLn
        [ catLine >> varLine (nameOf $ iv var)
        , specLine
        , parseBits, unparseBits
        , fMkExtended (iv var)
        , fParseExpansion (nameOf $ iv var)
        ]
  where

    iv = indexOf db

    catLine = fmt ("cat = " % int) (astCat ast)

    varLine cls = fmt ("variation = " % stext) cls

    specLine = "spec = variation.spec"

    parseBits = "parse_bits = variation.parse_bits"

    unparseBits = "unparse_bits = variation.unparse_bits"

    fMkRecord ix = do
        line "@classmethod"
        pyFunc "make_record" ["cls, val : " <> argOf ix] (nameOf ix) $ do
            fmt ("return " % stext % "(val)") (nameOf ix)

    fMkRecordUnsafe uaps = blocksLn [mapM_ overload uaps, func]
      where
        overload (name, var) = do
            let ix = indexOf db var
            line "@overload"
            line "@classmethod"
            pyFunc "make_record_unsafe"
                ["cls, uap : Literal['" <> name <> "'], arg : " <> argOf ix]
                (nameOf ix)
                "..."

        func = do
            line "@classmethod"
            pyFunc "make_record_unsafe"
                ["cls", "uap : Any", "arg : Any"]
                "Any"
                "return cls.uaps[uap](arg)"

    fMkDatablock arg = do
        line "@classmethod"
        pyFunc "make_datablock"
            ["cls, val : Union[" <> arg <> ", List[" <> arg <> "]]"]
            ("Datablock[" <> arg <> "]")
            "return Datablock(cls.cat, val)"

    fParseSingle arg = do
        line "@classmethod"
        pyFunc "parse"
            ["cls", "val : RawDatablock", "opt : ParsingOptions"]
            ("Datablock[" <> arg <> "]")
            "return cls._parse(val, opt) # type: ignore"

    fParseMultiple uaps msel = do
        line "@classmethod"
        pyFunc "parse"
            ["cls", "val : RawDatablock", "opt : ParsingOptions", "uap : " <> uapArg]
            "Any"
            "return cls._parse(val, opt, uap=uap)"
      where
        uapSelection = tList [T.pack (show name) | name <- fmap fst uaps]
        uapArg = case msel of
            Nothing -> "Literal[" <> uapSelection <> "]"
            Just _sel -> "Optional[Literal[" <> uapSelection <> "]] = None"

    fMkExtended ix = do
        line "@classmethod"
        pyFunc "make_extended"
            ["cls", "val : " <> argOf ix]
            (nameOf ix)
            (fmt ("return " % stext % "(val)") (nameOf ix))

    fParseExpansion cls = do
        line "@classmethod"
        pyFunc "parse"
            ["cls", "val : bytes", "opt : ParsingOptions"]
            cls $ do
                line "s = Bits.from_bytes(val)"
                fmt ("(rec, s2) = " % stext % ".parse_bits(s, opt)") cls
                pyIf "len(s2) != 0"
                    "raise AsterixError('unable to parse expansion')"
                line "return rec # type: ignore"

    fUap uaps msel = blocksLn
        [ pyDict "uaps" $ do
            (name, var) <- uaps
            pure $ fmt ("'" % stext % "': " % stext % ",")
                name
                (nameOf $ indexOf db var)
        , blocksLn $ case msel of
            Nothing ->
                [ "uap_selector_item = None"
                , "uap_selector_table = None"
                ]
            Just (UapSelector item table) ->
                [ fmt ("uap_selector_item = [" % stext % "]") (tList (fmap (T.pack . show) item))
                , pyDict "uap_selector_table" $ do
                    (val, name) <- table
                    pure $ fmt (int % ": '" % stext % "',") val name
                ]
        , do
            line "@classmethod"
            pyFunc "spec"
                ["cls", "val : Literal[" <> tList (fmap (T.pack . show . fst) uaps) <> "]" ]
                ("Any")
                "return cls.uaps[val]"
        ]

    fIsValid uaps _sel = do
        line "@classmethod"
        pyFunc "is_valid"
            ["cls", "arg : Union[" <> tList args <> "]"]
            ("bool")
            "return cls._is_valid(arg)"
      where
        args = do
            (_name, var) <- uaps
            pure $ nameOf $ indexOf db var

programSpecs :: VariationDb -> [Asterix] -> BlockM Builder ()
programSpecs db lst = blocksLn $ fmap (handleSpec db) lst

-- | Create 'manifest' of all defined specs.
programManifest :: [Asterix] -> BlockM Builder ()
programManifest specs = enclose "manifest = {" "}" $ do
    enclose "'CATS': {" "}," $ mconcat $ go "CAT"
    enclose "'REFS': {" "}," $ mconcat $ go "REF"
  where
    lst = do
        spec@(Asterix cat ed at) <- specs
        pure $ case at of
            AstCat _ -> ("CAT", (cat, (ed, specName spec)))
            AstRef _ -> ("REF", (cat, (ed, specName spec)))

    go :: Text -> [BlockM Builder ()]
    go t = do
        let candidates = [b | (a, b) <- lst, a == t]
        cat <- sort $ nub $ fmap fst candidates
        let hdr = fmt (int % ": {") cat
        pure $ enclose hdr "}," $ mconcat $ do
            (Edition eMaj eMin, cls) <- sortOn fst [b | (a, b) <- candidates, a == cat]
            let edition = sformat ("'" % int % "." % int % "'") eMaj eMin
            pure $ fmt stext ( edition <> ": " <> cls <> ",")

-}
-}

prog :: Convert (Augmented (t, Int)) => AsterixDb EMap -> EMap t -> BlockM Builder ()
prog db mp = do
    let go = convert . Augmented db
        blocks = fmap go (enumList mp)
    sequence_ (intersperse "" blocks)

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
    line $ "reference = \"" <> BL.fromText ref <> "\""
    line $ "version = \"" <> BL.fromText ver <> "\""
    ""
    "# Content set"
    ""
    prog db (dbContent db)
    ""
    "# Rule Content set"
    ""
    prog db (dbRuleContent db)
    ""
    "# Variation NonSpare and Item set"
    ""
    do
        let lst = flattenVarNonItem (dbVariation dbSet, dbNonSpare dbSet, dbItem dbSet)
            f = \case
                EVariation var -> convert (Augmented db var)
                ENonSpare nsp -> convert (Augmented db nsp)
                EItem item -> convert (Augmented db item)
        sequence_ $ intersperse "" $ fmap f lst
    {-
    ""
    "# Record set"
    ""
    sequence_ $ intersperse "" $ fmap (mkRecord db) $ enumList $ dbRecord db
    ""
    "# Expansion set"
    ""
    sequence_ $ intersperse "" $ fmap (mkExpansion db) $ enumList $ dbExpansion db
    ""
    "# Uap set"
    ""
    sequence_ $ intersperse "" $ fmap (mkUap db) $ enumList $ dbUap db
    ""
    "# Asterix spec set"
    ""
    sequence_ $ intersperse "" $ fmap (mkAstSpec db) $ enumList $ dbAstSpec db
    ""
    "# Aliases"
    ""
    sequence_ $ fmap mkAlias $ enumList $ dbAstSpec db
    ""
    "# Manifest"
    ""
    mkManifest specs
-}
  where
    specs :: [Asterix]
    specs = sort $ nub $ fmap deriveAsterix specs'

    dbSet :: AsterixDb Set
    dbSet = asterixDb specs

    db :: AsterixDb EMap
    db = enumDb dbSet
