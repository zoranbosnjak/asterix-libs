-- | Generate asterix 'python' source code.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Python where

import           Control.Monad
import           Control.Monad.Trans.RWS
import           Data.Coerce
import           Data.List               (inits, intersperse, nub, sort, sortOn)
import           Data.Maybe
import           Data.Scientific
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

alias :: Text -> Text -> BlockM Builder ()
alias = fmt (stext % ": TypeAlias = " % stext)

pyClass :: (Integral a, HasIndent (BlockM Builder b))
    => Text -> a -> Text -> BlockM Builder b -> BlockM Builder b
pyClass name ix baseClass' body = do
    fmt ("class " % stext % "_" % int % stext % ":")
        name ix baseClass
    indent body
  where
    baseClass = case baseClass' of
        "" -> ""
        _  -> sformat ("(" % stext % ")") baseClass'

pyFunc :: Text -> [Text] -> Text -> BlockM Builder () -> BlockM Builder ()
pyFunc name args rv body = do
    fmt ("def " % stext % stext % " -> " % stext % ":")
        name (fmtList "(" ")" id args) rv
    indent body

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
                "cv_arg: TypeAlias = int"
        A.ContentTable values -> do
            let f (a, b) = sformat (int % ": " % stext) a (quote b)
            out $ pyClass "Content" ix "ContentTable" $ do
                "cv_arg: TypeAlias = int"
                fmt ("cv_values = " % stext) (fmtList "{" "}" f values)
        A.ContentString st -> do
            out $ pyClass "Content" ix "ContentString" $ do
                "cv_arg: TypeAlias = Union[int, str]"
                fmt ("cv_string_type: TypeAlias = " % stext) $ case st of
                    A.StringAscii -> "StringAscii"
                    A.StringICAO  -> "StringICAO"
                    A.StringOctal -> "StringOctal"
        A.ContentInteger sig _cstr -> do
            out $ pyClass "Content" ix "ContentInteger" $ do
                "cv_arg: TypeAlias = int"
                fmt ("cv_signedness: TypeAlias = " % stext) $ case sig of
                    A.Signed   -> "Signed"
                    A.Unsigned -> "Unsigned"
        A.ContentQuantity sig lsb unit' _cstr -> do
            let unit = quote $ coerce unit'
            out $ pyClass "Content" ix "ContentQuantity" $ do
                fmt ("cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal[" %
                     stext % "]]]") unit
                fmt ("cv_signedness: TypeAlias = " % stext) $ case sig of
                    A.Signed   -> "Signed"
                    A.Unsigned -> "Unsigned"
                fmt ("cv_lsb = " % scifmt Generic Nothing)
                    (fromFloatDigits (A.evalNumber lsb :: Double))
                fmt ("cv_unit = " % stext) unit
                ""
                do
                    let arg = sformat
                            ("cv_unit : Optional[Literal[" % stext % "]] = None")
                            unit
                    pyFunc "as_quantity" ["self", arg] "float" $ do
                        "return self._as_quantity()"
        A.ContentBds bt -> do
            out $ pyClass "Content" ix "ContentBds" $ do
                "cv_arg: TypeAlias = int"
                case bt of
                    A.BdsWithAddress ->
                        "cv_bds_type = BdsWithAddress"
                    A.BdsAt Nothing ->
                        "cv_bds_type = (BdsAt, None)"
                    A.BdsAt (Just addr) -> fmt
                        ("cv_bds_type = (BdsAt, " % int % ")") (coerce addr :: Int)

instance Node (A.Rule A.Content) where
    focus = lRuleContent
    node ix = \case
        A.ContextFree content -> do
            ref <- walk content
            out $ pyClass "RuleContent" ix "RuleContentContextFree" $ do
                fmt ("cv_arg: TypeAlias = Content_" % int % ".cv_arg") ref
                fmt ("cv_content: TypeAlias = Content_" % int) ref
                ""
                "@property"
                pyFunc "content" ["self"] (sformat ("Content_" % int) ref) $ do
                    "return self._get_content() # type: ignore"
        A.Dependent items dv cases -> do
            refDv <- walk dv
            refs <- forM cases $ \(a, b) -> do
                ref <- walk b
                pure (a, ref)
            out $ pyClass "RuleContent" ix "RuleContentDependent" $ do
                "cv_arg: TypeAlias = Union["
                indent $ do
                    fmt ("Tuple[None, Content_" % int % ".cv_arg],") refDv
                    forM_ refs $ \(a, ref) -> do
                        let f = sformat ("Literal[" % int % "]")
                        fmt ("Tuple[" % stext % ", Content_" % int % ".cv_arg],")
                            (fmtList "Tuple[" "]" f a) ref
                "]"
                let f1 (A.ItemPath path) = fmtList "[" "]" f2 path
                    f2 (A.ItemName name) = quote name
                fmt ("cv_depends_on = " % stext)
                    (fmtList "[" "]" f1 items)
                fmt ("cv_default_content: TypeAlias = Content_" % int) refDv
                "cv_cases = ["
                indent $ forM_ refs $ \(a, ref) -> do
                    let val = fmtList "[" "]" (sformat int) a
                    fmt ("(" % stext % ", Content_" % int % "),") val ref
                "]"
                ""
                "@overload"
                pyFunc "content" ["self", "arg : Literal[None]"]
                    (sformat ("Content_" % int) refDv) $ do
                        "..."
                forM_ refs $ \(a, ref) -> do
                    "@overload"
                    let f = sformat ("Literal[" % int % "]")
                        arg = fmtList "Tuple[" "]" f a
                    pyFunc "content" ["self", "arg : " <> arg]
                        (sformat ("Content_" % int) ref) $ do
                            "..."
                pyFunc "content" ["self", "arg : Any"] "Any" $ do
                    "return self._get_content(arg)"

instance Node Variation where
    focus = lVariation
    node ix = \case
        A.Element o n rule -> do
            ref <- walk rule
            mRef2 <- case rule of
                A.Dependent {}        -> pure Nothing
                A.ContextFree content -> Just <$> walk content
            out $ pyClass "Variation" ix "Element" $ do
                fmt ("cv_arg: TypeAlias = RuleContent_" % int % ".cv_arg") ref
                fmt ("cv_bit_offset8 = " % int) (coerce o :: Int)
                fmt ("cv_bit_size = " % int) (coerce n :: Int)
                fmt ("cv_rule = RuleContent_" % int) ref
                ""
                "@classmethod"
                let arg = sformat ("Variation_" % int % ".cv_arg") ix
                    rv = sformat("Variation_" % int) ix
                pyFunc "create" ["cls", "arg: " <> quote arg] (quote rv) $ do
                    "return cls._create(arg) # type: ignore"
                ""
                "@property"
                pyFunc "rule" ["self"]
                    (sformat ("RuleContent_" % int) ref) $ do
                        "return self._get_rule() # type: ignore"
                case mRef2 of
                    Nothing -> pure ()
                    Just ref2 -> do
                        ""
                        "# shortcut"
                        "@property"
                        pyFunc "content" ["self"]
                            (sformat ("Content_" % int) ref2) $ do
                                "return self.rule.content"

        A.Group o lst -> do
            refList <- forM lst $ \i -> do
                ref <- walk i
                pure (ref, bitSizeOfItem i)
            refDict <- forM lst $ \case
                A.Spare _ _ -> pure Nothing
                A.Item (A.NonSpare name _ var _) -> do
                    ref <- walk var
                    pure $ Just (name, ref)
            let n = sum $ fmap bitSizeOfItem lst
            out $ pyClass "Variation" ix "Group" $ do
                let f1 :: Maybe (A.ItemName, Int) -> Text
                    f1 = \case
                        Nothing -> "int"
                        Just (name', ref) ->
                            let arg = sformat ("RuleVariation_" % int % ".cv_arg") ref
                                name = quote $ coerce name'
                                namedArg = sformat ("Tuple[Literal[" % stext % "], " %
                                                    stext % "]")
                                    name arg
                            in sformat ("Union[" % stext % ", " % stext % "]") arg
                                   namedArg
                fmt ("cv_arg_group: TypeAlias = " % stext)
                    (fmtList "Tuple[" "]" f1 refDict)
                let ag = sformat ("Variation_" % int % ".cv_arg_group") ix
                fmt ("cv_arg: TypeAlias = Union[int, " % stext % "]") (quote ag)
                fmt ("cv_bit_offset8 = " % int) (coerce o :: Int)
                fmt ("cv_bit_size = " % int) n
                let f2 (ref, bitSize) = sformat ("(Item_" % int % ", " % int % ")")
                        ref  bitSize
                fmt ("cv_items_list = " % stext) (fmtList "[" "]" f2 refList)
                let f3 (name, ref) = sformat (stext % ": RuleVariation_" % int)
                        (quote $ coerce name) ref
                fmt ("cv_items_dict = " % stext) (fmtList "{" "}" f3
                                                  (catMaybes refDict))
                ""
                case catMaybes refDict of
                    [] -> pure ()
                    [(name', ref)] -> do
                        let name2 = quote $ coerce name'
                            arg2 = sformat ("key : Literal[" % stext % "]") name2
                            rv2 = sformat ("RuleVariation_" % int) ref
                        "@classmethod"
                        pyFunc "spec" ["cls", arg2] rv2 $ do
                            "return cls._spec(arg) # type: ignore"
                        ""
                        do
                            let name = quote $ coerce name'
                                arg = sformat ("key : Literal[" % stext % "]") name
                                rv = sformat ("RuleVariation_" % int) ref
                            pyFunc "get_item" ["self", arg] rv $ do
                                "return self._get_item(key) # type: ignore"
                        ""
                        do
                            let arg3 = sformat ("Variation_" % int % ".cv_arg") ix
                                rv = sformat ("'Variation_" % int % "'") ix
                            "@classmethod"
                            pyFunc "create" ["cls", "arg:" <> quote arg3] rv $ do
                                "return cls._create(arg) # type: ignore"
                    refDict' -> do
                        forM_ refDict' $ \(name', ref) -> do
                            let name = quote $ coerce name'
                                arg = sformat ("key : Literal[" % stext % "]") name
                                rv = sformat ("RuleVariation_" % int) ref
                            "@overload"
                            "@classmethod"
                            pyFunc "spec" ["cls", arg] rv $ do
                                "..."
                        "@classmethod"
                        let f4 (a, _) = sformat ("Literal[" % stext % "]")
                                (quote $ coerce a)
                            arg4 = sformat("key : Union" % stext)
                                (fmtList "[" "]" f4 refDict')
                        pyFunc "spec" ["cls", arg4] "Any" $ do
                            "return cls._spec(key)"
                        ""
                        forM_ refDict' $ \(name', ref) -> do
                            let name = quote $ coerce name'
                                arg = sformat ("key : Literal[" % stext % "]") name
                                rv = sformat ("RuleVariation_" % int) ref
                            "@overload"
                            pyFunc "get_item" ["self", arg] rv $ do
                                "..."
                        pyFunc "get_item" ["self", "key : Any"] "Any" $ do
                            "return self._get_item(key)"
                        ""
                        "@classmethod"
                        let arg2 = sformat ("Variation_" % int % ".cv_arg") ix
                            rv = sformat ("'Variation_" % int % "'") ix
                        pyFunc "create" ["cls", "arg: " <> quote arg2] rv $ do
                            "return cls._create(arg) # type: ignore"

        A.Extended lst -> do
            groups <- forM (unconcatMaybe lst) $ \group -> do
                forM group $ \case
                    Nothing -> pure Nothing
                    Just i -> Just <$> case i of
                        A.Spare _ _ -> pure Nothing
                        A.Item (A.NonSpare name _ rule _) -> do
                            ref <- walk rule
                            pure $ Just (name, ref)
            refList <- forM (unconcatMaybe lst) $ \group -> do
                forM group $ \case
                    Nothing -> pure Nothing
                    Just i -> do
                        ref <- walk i
                        pure $ Just (ref, bitSizeOfItem i)
            let num_of_items = length $ concat $ flip fmap groups $ \group -> do
                    i <- group
                    case i of
                        Just (Just (_,_)) -> [()]
                        _                 -> []
            out $ pyClass "Variation" ix "Extended" $ do
                forM_ (zip [1::Int ..] groups) $ \(gi, group) -> do
                    let f = \case
                            Nothing -> "None"
                            Just Nothing -> "int"
                            Just (Just (name, ref)) ->
                                let a1 = sformat ("RuleVariation_" % int % ".cv_arg")
                                        ref
                                    a2 = sformat ("Tuple[Literal[" % stext % "], " %
                                                  stext % "]")
                                        (quote $ coerce name) a1
                                    a3 = sformat ("Union[" % stext % ", " % stext %
                                                  "]") a1 a2
                                in a3
                        arg = fmtList "Union[int, Tuple[" "]]" f group
                    fmt ("cv_arg_group_" % int % ": TypeAlias = " % stext) gi arg
                "cv_arg: TypeAlias = Union["
                indent $ forM_ (tail $ inits [1 .. length groups]) $ \gi -> do
                    let f = sformat ("Variation_" % int % ".cv_arg_group_" % int) ix
                    fmt stext (fmtList "Tuple[" "]," (quote . f) gi)
                "]"
                let f1 = \case
                        Nothing -> "None"
                        Just (ref, bitSize) -> sformat ("(Item_" % int % ", " %
                            int % ")") ref  bitSize
                fmt ("cv_items_list = " % stext)
                    (fmtList "[" "]" (fmtList "[" "]" f1) refList)
                ""
                let arg2 = sformat ("Variation_" % int % ".cv_arg") ix
                    rv = sformat ("'Variation_" % int % "'") ix
                "@classmethod"
                pyFunc "create" ["cls", "arg: " <> quote arg2] rv $ do
                    "return cls._create(arg) # type: ignore"
                ""
                forM_ (zip [1::Int ..] groups) $ \(gi, group) -> do
                    forM_ group $ \case
                        Just (Just (name, ref)) -> do
                            let arg = sformat ("key : Literal[" % stext % "]")
                                    (quote $ coerce name)
                                rv2 = case gi < 2 of
                                    True -> sformat ("RuleVariation_" % int) ref
                                    False -> sformat ("Optional[RuleVariation_" % int
                                                      % "]") ref
                            case num_of_items of
                                1 -> "@overload # type: ignore"
                                _ -> "@overload"
                            pyFunc "get_item" ["self", arg] rv2 $ do
                                "..."
                        _ -> pure ()
                pyFunc "get_item" ["self", "key : Any"] "Any" $ do
                    "return self._get_item(key)"

        A.Repetitive rt var -> do
            ref <- walk var
            out $ pyClass "Variation" ix "Repetitive" $ do
                fmt ("cv_arg: TypeAlias = List[Variation_" % int % ".cv_arg]") ref
                case rt of
                    A.RepetitiveFx -> "cv_rep_bytes = None"
                    A.RepetitiveRegular n -> do
                        fmt ("cv_rep_bytes = " % int) (coerce n :: Int)
                fmt ("cv_variation: TypeAlias = Variation_" % int) ref
                ""
                let arg2 = sformat ("Variation_" % int % ".cv_arg") ix
                    rv = sformat ("'Variation_" % int % "'") ix
                "@classmethod"
                pyFunc "create" ["cls", "arg: " <> quote arg2] rv $ do
                    "return cls._create(arg) # type: ignore"
                ""
                pyFunc "get_list" ["self"]
                    (sformat ("List[Variation_" % int % "]") ref) $ do
                        "return self._get_list() # type: ignore"

        A.Explicit met -> do
            out $ pyClass "Variation" ix "Explicit" $ do
                "cv_arg: TypeAlias = bytes"
                fmt ("cv_explicit_type: TypeAlias = " % stext) $ case met of
                    Nothing                  -> "None"
                    Just A.ReservedExpansion -> "ReservedExpansion"
                    Just A.SpecialPurpose    -> "SpecialPurpose"
                ""
                let arg2 = sformat ("Variation_" % int % ".cv_arg") ix
                    rv = sformat ("'Variation_" % int % "'") ix
                "@classmethod"
                pyFunc "create" ["cls", "arg: " <> quote arg2] rv $ do
                    "return cls._create(arg) # type: ignore"

        A.Compound lst -> do
            refList <- forM lst $ \case
                Nothing -> pure "None"
                Just i -> sformat ("NonSpare_" % int) <$> walk i
            refDict <- forM (catMaybes lst) $ \case
                nsp@(A.NonSpare name _ _ _) -> (name,) <$> walk nsp
            out $ pyClass "Variation" ix "Compound" $ do
                "cv_arg = TypedDict('cv_arg', {"
                indent $ forM_ refDict $ \(name, ref) -> do
                    fmt (stext % ": NonSpare_" % int % ".cv_arg,")
                        (quote $ coerce name) ref
                "}, total=False)"
                fmt ("cv_fspec_max_bytes = " % int) (fspecMaxBytes lst)
                fmt ("cv_items_list = " % stext) (fmtList "[" "]" id refList)
                let f (A.ItemName name, i) = sformat
                        (stext % ": NonSpare_" % int)
                        (quote name) i
                fmt ("cv_items_dict = " % stext) (fmtList "{" "}" f refDict)
                ""
                case refDict of
                    [] -> pure ()
                    [(name', ref)] -> do
                        let name = quote $ coerce name'
                            arg = sformat ("key : Literal[" % stext % "]") name
                            rv = sformat ("NonSpare_" % int) ref
                            rv2 = sformat ("'Variation_" % int % "'") ix
                        "@classmethod"
                        pyFunc "spec" ["cls", arg] rv $ do
                            "return cls._spec(arg) # type: ignore"
                        ""
                        pyFunc "get_item" ["self", arg] ("Optional[" <> rv <> "]") $ do
                            "return self._get_item(key) # type: ignore"
                        ""
                        let arg2 = sformat ("val : NonSpare_" % int % ".cv_arg")
                                ref
                        pyFunc "set_item" ["self", arg, arg2] rv2 $ do
                            "return self._set_item(key, val) # type: ignore"
                        ""
                        pyFunc "del_item" ["self", arg] rv2 $ do
                            "return self._del_item(key) # type: ignore"
                    _ -> do
                        forM_ refDict $ \(name', ref) -> do
                            let name = quote $ coerce name'
                                arg = sformat ("key : Literal[" % stext % "]") name
                                rv = sformat ("NonSpare_" % int) ref
                            "@overload"
                            "@classmethod"
                            pyFunc "spec" ["cls", arg] rv $ do
                                "..."
                        "@classmethod"
                        let f1 (a, _) = sformat ("Literal[" % stext % "]")
                                (quote $ coerce a)
                            arg = sformat("key : Union" % stext)
                                (fmtList "[" "]" f1 refDict)
                        pyFunc "spec" ["cls", arg] "Any" $ do
                            "return cls._spec(key)"
                        ""
                        -- get_item
                        forM_ refDict $ \(name, ref) -> do
                            "@overload"
                            let arg2 = sformat ("Literal[" % stext % "]")
                                    (quote $ coerce name)
                                rv2 = sformat ("Optional[NonSpare_" % int % "]") ref
                            pyFunc "get_item" ["self", "key : " <> arg2] rv2 $ do
                                "..."
                        pyFunc "get_item" ["self", "key : Any"] "Any" $ do
                            "return self._get_item(key)"
                        ""
                        -- set_item
                        forM_ refDict $ \(name, ref) -> do
                            "@overload"
                            let key = sformat ("Literal[" % stext % "]")
                                    (quote $ coerce name)
                                arg2 = sformat ("NonSpare_" % int % ".cv_arg") ref
                                rv2 = quote $ sformat ("Variation_" % int) ix
                            pyFunc "set_item" [ "self" , "key : " <> key
                                , "val : " <> arg2] rv2 $ do
                                    "..."
                        pyFunc "set_item" ["self", "key : Any", "val : Any"] "Any" $ do
                            "return self._set_item(key, val)"
                        ""
                        -- del_item
                        forM_ refDict $ \(name, _ref) -> do
                            "@overload"
                            let key = sformat ("Literal[" % stext % "]")
                                    (quote $ coerce name)
                                rv2 = quote $ sformat ("Variation_" % int) ix
                            pyFunc "del_item" [ "self" , "key : " <> key] rv2 $ do
                                    "..."
                        pyFunc "del_item" ["self", "key : Any"] "Any" $ do
                            "return self._del_item(key)"
                ""
                let arg2 = sformat ("Variation_" % int % ".cv_arg") ix
                    rv = sformat ("'Variation_" % int % "'") ix
                "@classmethod"
                pyFunc "create" ["cls", "arg: " <> quote arg2] rv $ do
                    "return cls._create(arg) # type: ignore"

instance Node (A.Rule Variation) where
    focus = lRuleVariation
    node ix = \case
        A.ContextFree var -> do
            let rv = sformat ("RuleVariation_" % int) ix
            ref <- walk var
            out $ pyClass "RuleVariation" ix "RuleVariationContextFree" $ do
                fmt ("cv_arg: TypeAlias = Variation_" % int % ".cv_arg") ref
                fmt ("cv_variation: TypeAlias = Variation_" % int) ref
                ""
                "@classmethod"
                pyFunc "create" ["cls", "arg : " <> quote (rv <> ".cv_arg")]
                    (quote rv) $ do
                        "return cls._create(arg) # type: ignore"
                ""
                "@property"
                pyFunc "variation" ["self"] (sformat ("Variation_" % int) ref) $ do
                    "return self.arg # type: ignore"
        A.Dependent items dv cases -> do
            let rv = sformat ("RuleVariation_" % int) ix
            refDv <- walk dv
            refs <- forM cases $ \(a, b) -> do
                ref <- walk b
                pure (a, ref)
            out $ pyClass "RuleVariation" ix "RuleVariationDependent" $ do
                "cv_arg: TypeAlias = Union["
                indent $ forM_ (refDv : fmap snd refs) $ \ref -> do
                    fmt ("Variation_" % int % ".cv_arg,") ref
                "]"
                let f1 (A.ItemPath path) = fmtList "[" "]" f2 path
                    f2 (A.ItemName name) = quote name
                fmt ("cv_depends_on = " % stext)
                    (fmtList "[" "]" f1 items)
                fmt ("cv_default_variation: TypeAlias = Variation_" % int) refDv
                "cv_cases = ["
                indent $ forM_ refs $ \(a, ref) -> do
                    let val = fmtList "[" "]" (sformat int) a
                    fmt ("(" % stext % ", Variation_" % int % "),") val ref
                "]"
                ""
                forM_ refs $ \(a, ref) -> do
                    let val = sformat ("Literal[" % int % "]")
                        key = "key : " <> fmtList "Tuple[" "]" val a
                        var = sformat("Variation_" % int) ref
                    "@overload"
                    "@classmethod"
                    pyFunc "variation" ["cls", key] var $ do
                        "..."
                ""
                "@classmethod"
                pyFunc "variation" ["cls", "key : Any"] "Any" $ do
                    "return cls._variation(key)"
                ""
                "@classmethod"
                pyFunc "create" ["cls", "arg : " <> quote (rv <> ".cv_arg")]
                    (quote rv) $ do
                        "return cls._create(arg) # type: ignore"

instance Node NonSpare where
    focus = lNonSpare
    node ix (A.NonSpare name title rule _doc) = do
        ref <- walk rule
        mRef2 <- case rule of
            A.Dependent {}    -> pure Nothing
            A.ContextFree var -> Just <$> walk var
        let clsName = sformat ("NonSpare_" % int) ix
        out $ pyClass "NonSpare" ix "NonSpare" $ do
            fmt ("cv_arg: TypeAlias = RuleVariation_" % int % ".cv_arg") ref
            fmt ("cv_name = " % stext) (quote $ coerce name)
            fmt ("cv_title = " % stext) (quote $ coerce title)
            fmt ("cv_rule: TypeAlias = RuleVariation_" % int) ref
            ""
            "@classmethod"
            pyFunc "create" ["cls", "arg : " <> quote(clsName <> ".cv_arg")]
                (quote clsName) $ do
                    "return cls._create(arg) # type: ignore"
            ""
            "@property"
            pyFunc "rule" ["self"] (sformat ("RuleVariation_" % int) ref) $ do
                "return self.arg # type: ignore"
            case mRef2 of
                Nothing -> pure ()
                Just ref2 -> do
                    ""
                    "# shortcut to variation"
                    "@property"
                    pyFunc "variation" ["self"] (sformat ("Variation_" % int) ref2) $
                        do
                            "return self.rule.variation"

instance Node Item where
    focus = lItem
    node ix = \case
        A.Spare o n -> do
            out $ pyClass "Item" ix "Spare" $ do
                "cv_arg: TypeAlias = int"
                fmt ("cv_bit_offset8 = " % int) (coerce o :: Int)
                fmt ("cv_bit_size = " % int) (coerce n :: Int)
        A.Item nsp -> do
            ref <- walk nsp
            let clsName = sformat ("Item_" % int) ix
            out $ pyClass "Item" ix "Item" $ do
                fmt ("cv_arg: TypeAlias = NonSpare_" % int % ".cv_arg") ref
                fmt ("cv_non_spare: TypeAlias = NonSpare_" % int) ref
                ""
                "@classmethod"
                pyFunc "create" ["cls", "arg : " <> quote (clsName <> ".cv_arg")]
                    (quote clsName) $ do
                        "return cls._create(arg) # type: ignore"

instance Node UapItem where
    focus = lUapItem
    node ix = \case
        A.UapItem nsp -> do
            ref <- walk nsp
            out $ pyClass "UapItem" ix "UapItem" $ do
                fmt ("cv_non_spare: TypeAlias = NonSpare_" % int) ref
        A.UapItemSpare -> do
            out $ pyClass "UapItem" ix "UapItemSpare" "pass"
        A.UapItemRFS -> do
            out $ pyClass "UapItem" ix "UapItemRFS" "pass"

instance Node Record where
    focus = lRecord
    node ix (Record items) = do
        let rfs = hasRFS (Record items)
        refList <- mapM walk items
        refDict <- catMaybes <$> forM items (\case
            A.UapItem nsp@(A.NonSpare name _ _ _) -> do
                ref <- walk nsp
                pure $ Just (quote $ coerce name, ref)
            A.UapItemSpare -> pure Nothing
            A.UapItemRFS -> pure Nothing)
        out $ pyClass "Record" ix "Record" $ do
            "cv_arg = TypedDict('cv_arg', {"
            indent $ forM_ refDict $ \(name, ref) -> do
                fmt (stext % ": NonSpare_" % int % ".cv_arg,") name ref
            "}, total=False)"
            when rfs $ do
                "cv_union: TypeAlias = Union["
                indent $ forM_ refDict $ \(name, ref) -> do
                    fmt ("Tuple[Literal[" % stext % "], NonSpare_" % int % ".cv_arg],")
                        name ref
                "]"
            fmt ("cv_fspec_max_bytes = " % int) (fspecMaxBytes items)
            do
                let f = sformat ("UapItem_" % int)
                fmt ("cv_items_list = " % stext) (fmtList "[" "]" f refList)
            do
                let f (name, ref) = sformat (stext % ": NonSpare_" % int)
                        name ref
                fmt ("cv_items_dict = " % stext) (fmtList "{" "}" f refDict)
            ""
            case refDict of
                [] -> pure ()
                [(name, ref)] -> do
                    let arg = sformat ("key : Literal[" % stext % "]") name
                        rv = sformat ("NonSpare_" % int) ref
                        rv2 = sformat ("'Record_" % int % "'") ix
                    "@classmethod"
                    pyFunc "spec" ["cls", arg] rv $ do
                        "return cls._spec(arg) # type: ignore"
                    ""
                    pyFunc "get_item" ["self", arg] ("Optional[" <> rv <> "]") $ do
                        "return self._get_item(key) # type: ignore"
                    ""
                    let arg2 = sformat ("val : NonSpare_" % int % ".cv_arg")
                            ref
                    pyFunc "set_item" ["self", arg, arg2] rv2 $ do
                        "return self._set_item(key, val) # type: ignore"
                    ""
                    pyFunc "del_item" ["self", arg] rv2 $ do
                        "return self._del_item(key) # type: ignore"
                    when rfs $ do
                        ""
                        pyFunc "get_rfs_item" ["self", arg] ("List[" <> rv <> "]") $ do
                            "return self._get_rfs_item(key) # type: ignore"
                _ -> do
                    forM_ refDict $ \(name, ref) -> do
                        let arg = sformat ("key : Literal[" % stext % "]") name
                            rv = sformat ("NonSpare_" % int) ref
                        "@overload"
                        "@classmethod"
                        pyFunc "spec" ["cls", arg] rv $ do
                            "..."
                    "@classmethod"
                    let f (a, _) = sformat ("Literal[" % stext % "]") a
                        arg2 = sformat("key : Union" % stext) (fmtList "[" "]" f refDict)
                    pyFunc "spec" ["cls", arg2] "Any" $ do
                        "return cls._spec(key)"
                    ""
                    forM_ refDict $ \(name, ref) -> do
                        "@overload"
                        let arg = sformat ("Literal[" % stext % "]") name
                            rv2 = sformat ("Optional[NonSpare_" % int % "]") ref
                        pyFunc "get_item" ["self", "key : " <> arg] rv2 $ do
                            "..."
                    pyFunc "get_item" ["self", "key : Any"] "Any" $ do
                        "return self._get_item(key)"
                    ""
                    forM_ refDict $ \(name, ref) -> do
                        "@overload"
                        let arg = sformat ("Literal[" % stext % "]") name
                            arg3 = sformat ("NonSpare_" % int % ".cv_arg") ref
                            rv2 = sformat ("'Record_" % int % "'") ix
                        pyFunc "set_item" ["self", "key : " <> arg,
                                            "val : " <> arg3] rv2 $ do
                            "..."
                    pyFunc "set_item" ["self", "key : Any", "val : Any"] "Any" $ do
                        "return self._set_item(key, val)"
                    ""
                    forM_ refDict $ \(name, _ref) -> do
                        "@overload"
                        let arg = sformat ("Literal[" % stext % "]") name
                            rv2 = sformat ("'Record_" % int % "'") ix
                        pyFunc "del_item" ["self", "key : " <> arg] rv2 $ do
                            "..."
                    pyFunc "del_item" ["self", "key : Any"] "Any" $ do
                        "return self._del_item(key)"
                    when rfs $ do
                        ""
                        forM_ refDict $ \(name, ref) -> do
                            "@overload"
                            let arg = sformat ("Literal[" % stext % "]") name
                                rv2 = sformat ("List[NonSpare_" % int % "]") ref
                            pyFunc "get_rfs_item" ["self", "arg : " <> arg] rv2 $ do
                                "..."
                        pyFunc "get_rfs_item" ["self", "arg : Any"] "Any" $ do
                            "return self._get_rfs_item(arg)"
            do
                ""
                let self = sformat ("Record_" % int) ix
                    arg_dict = "arg: " <> quote (self <> ".cv_arg")
                    arg_rfs = "rfs : Optional[List["
                        <> quote (self <> ".cv_union") <> "]] = None"
                    args
                        | rfs = ["cls", arg_dict, arg_rfs]
                        | otherwise = ["cls", arg_dict]
                    rv = sformat ("'Record_" % int % "'") ix
                "@classmethod"
                pyFunc "create" args rv $ case rfs of
                    True  -> "return cls._create(arg, rfs) # type: ignore"
                    False -> "return cls._create(arg) # type: ignore"
            do
                ""
                let r = sformat ("Record_" % int) ix
                    rv = sformat ("Union[ValueError, Tuple[" % stext % ", Bits]]")
                        (quote r)
                "@classmethod"
                pyFunc "parse" ["cls", "bs : Bits"] rv $ do
                    "return cls._parse(bs) # type: ignore"

instance Node Uap where
    focus = lUap
    node ix = \case
        A.Uap record -> do
            ref <- walk record
            out $ pyClass "Uap" ix "UapSingle" $ do
                fmt ("cv_arg: TypeAlias = Record_" % int) ref
                fmt ("cv_record: TypeAlias = Record_" % int) ref
                ""
                "@classmethod"
                let cls = sformat ("Record_" % int) ref
                    rv = sformat ("Union[ValueError, List[" % stext % "]]") cls
                pyFunc "parse" ["cls", "bs : Bits"] rv $ do
                    "return cls._parse(bs)"
        A.Uaps lst msel -> do
            refs <- forM lst $ \(A.UapName uapName, record) -> do
                ref <- walk record
                pure (quote uapName, ref)
            out $ pyClass "Uap" ix "UapMultiple" $ do
                let f (_name, ref) = sformat ("Record_" % int) ref
                fmt ("cv_arg: TypeAlias = Union" % stext) (fmtList "[" "]" f refs)
                let f1 (name, ref) = sformat (stext % ": Record_" % int) name ref
                fmt ("cv_uaps = " % stext) (fmtList "{" "}" f1 refs)
                let f2 (i, A.UapName uapName) = sformat
                        (int % ": " % stext)
                        i (quote uapName)
                fmt ("cv_selector = " % stext) $ case msel of
                    Nothing -> "None"
                    Just (A.UapSelector (A.ItemPath item) table) ->
                        "("
                        <> fmtList "[" "]" quote (coerce item)
                        <> ", "
                        <> fmtList "{" "}" f2 table
                        <> ")"
                do
                    ""
                    forM_ refs $ \(name, ref) -> do
                        "@overload"
                        "@classmethod"
                        let arg = sformat ("Literal[" % stext % "]") name
                            rv = sformat ("Record_" % int) ref
                        pyFunc "spec" ["cls", "key : " <> arg] rv $ do
                            "..."
                    "@classmethod"
                    pyFunc "spec" ["cls", "key : Any"] "Any" $ do
                        "return cls._spec(key)"
                do
                    ""
                    forM_ refs $ \(name, ref) -> do
                        "@overload"
                        "@classmethod"
                        let arg = sformat ("uap : Literal[" % stext % "]") name
                            rv = sformat ("Union[ValueError, List[Record_" % int
                                            % "]]") ref
                        pyFunc "parse" ["cls", arg, "bs : Bits"] rv $ do
                            "..."
                    "@classmethod"
                    pyFunc "parse" ["cls", "uap : str", "bs : Bits"] "Any" $ do
                        "return cls._parse(uap, bs)"
                do
                    ""
                    "@classmethod"
                    let rv = sformat ("List[List[Uap_" % int % ".cv_arg]]") ix
                    pyFunc "parse_any_uap" ["cls", "bs : Bits"] (quote rv) $ do
                        "return cls._parse_any_uap(bs)"

instance Node Expansion where
    focus = lExpansion
    node ix (Expansion mn lst) = do
        refList <- forM lst $ \case
            Nothing -> pure "None"
            Just nsp -> sformat ("NonSpare_" % int) <$> walk nsp
        refDict <- forM (catMaybes lst) $ \case
            nsp@(A.NonSpare name _ _ _) -> (name,) <$> walk nsp
        out $ pyClass "Expansion" ix "Expansion" $ do
            "cv_arg = TypedDict('cv_arg', {"
            indent $ forM_ refDict $ \(name, ref) -> do
                fmt (stext % ": NonSpare_" % int % ".cv_arg,")
                    (quote $ coerce name) ref
            "}, total=False)"
            fmt ("cv_type = " % stext) $ case mn of
                Nothing -> sformat ("(FspecFx, " % int % ")")
                    (fspecMaxBytes lst)
                Just n -> sformat ("(FspecFixed, " % int % ")")
                    (coerce n :: Int)
            fmt ("cv_items_list = " % stext) (fmtList "[" "]" id refList)
            do
                let f (A.ItemName name, i) = sformat
                        (stext % ": NonSpare_" % int) (quote name) i
                fmt ("cv_items_dict = " % stext) (fmtList "{" "}" f refDict)
            ""
            case refDict of
                [] -> pure ()
                [(name', ref)] -> do
                    let name = quote $ coerce name'
                        arg = sformat ("key : Literal[" % stext % "]") name
                        rv = sformat ("NonSpare_" % int) ref
                        rv2 = sformat ("'Expansion_" % int % "'") ix
                    "@classmethod"
                    pyFunc "spec" ["cls", arg] rv $ do
                        "return cls._spec(arg) # type: ignore"
                    ""
                    pyFunc "get_item" ["self", arg] ("Optional[" <> rv <> "]") $ do
                        "return self._get_item(key) # type: ignore"
                    ""
                    let arg2 = sformat ("val : NonSpare_" % int % ".cv_arg")
                            ref
                    pyFunc "set_item" ["self", arg, arg2] rv2 $ do
                        "return self._set_item(key, val) # type: ignore"
                    ""
                    pyFunc "del_item" ["self", arg] rv2 $ do
                        "return self._del_item(key) # type: ignore"
                _ -> do
                    forM_ refDict $ \(name', ref) -> do
                        let name = quote $ coerce name'
                            arg = sformat ("key : Literal[" % stext % "]") name
                            rv = sformat ("NonSpare_" % int) ref
                        "@overload"
                        "@classmethod"
                        pyFunc "spec" ["cls", arg] rv $ do
                            "..."
                    "@classmethod"
                    let f1 (a, _) = sformat ("Literal[" % stext % "]")
                            (quote $ coerce a)
                        arg = sformat("key : Union" % stext)
                            (fmtList "[" "]" f1 refDict)
                    pyFunc "spec" ["cls", arg] "Any" $ do
                        "return cls._spec(key)"
                    ""
                    -- get_item
                    forM_ refDict $ \(name, ref) -> do
                        "@overload"
                        let arg2 = sformat ("Literal[" % stext % "]")
                                (quote $ coerce name)
                            rv2 = sformat ("Optional[NonSpare_" % int % "]") ref
                        pyFunc "get_item" ["self", "key : " <> arg2] rv2 $ do
                            "..."
                    pyFunc "get_item" ["self", "key : Any"] "Any" $ do
                        "return self._get_item(key)"
                    ""
                    -- set_item
                    forM_ refDict $ \(name, ref) -> do
                        "@overload"
                        let key = sformat ("Literal[" % stext % "]")
                                (quote $ coerce name)
                            arg2 = sformat ("NonSpare_" % int % ".cv_arg") ref
                            rv2 = quote $ sformat ("Expansion_" % int) ix
                        pyFunc "set_item" [ "self" , "key : " <> key
                            , "val : " <> arg2] rv2 $ do
                                "..."
                    pyFunc "set_item" ["self", "key : Any", "val : Any"] "Any" $ do
                        "return self._set_item(key, val)"
                    ""
                    -- del_item
                    forM_ refDict $ \(name, _ref) -> do
                        "@overload"
                        let key = sformat ("Literal[" % stext % "]")
                                (quote $ coerce name)
                            rv2 = quote $ sformat ("Expansion_" % int) ix
                        pyFunc "del_item" [ "self" , "key : " <> key] rv2 $ do
                                "..."
                    pyFunc "del_item" ["self", "key : Any"] "Any" $ do
                        "return self._del_item(key)"
            ""
            do
                let arg2 = sformat ("Expansion_" % int % ".cv_arg") ix
                    rv = sformat ("'Expansion_" % int % "'") ix
                "@classmethod"
                pyFunc "create" ["cls", "arg: " <> quote arg2] rv $ do
                    "return cls._create(arg) # type: ignore"
            do
                ""
                let r = sformat ("Expansion_" % int) ix
                    rv = sformat ("Union[ValueError, Tuple[" % stext % ", Bits]]")
                        (quote r)
                "@classmethod"
                pyFunc "parse" ["cls", "bs : Bits"] rv $ do
                    "return cls._parse(bs) # type: ignore"

instance Node Asterix where
    focus = lAsterix
    node ix = \case
        AsterixBasic cat (A.Edition a b) uap -> do
            ref <- walk uap
            mRef2 <- case uap of
                A.Uaps _ _   -> pure Nothing
                A.Uap record -> Just <$> walk record
            out $ pyClass "Asterix" ix "AstCat" $ do
                fmt ("cv_category = " % int) (coerce cat :: Int)
                fmt ("cv_edition = (" % int % ", " % int % ")") a b
                fmt ("cv_uap: TypeAlias = Uap_" % int) ref
                when (isJust mRef2) $ do
                    "cv_record: TypeAlias = cv_uap.cv_record # shortcut"

                ""
                "@classmethod"
                let arg = sformat ("records : List[Uap_" % int % ".cv_arg]") ref
                    rv = quote $ sformat ("Asterix_" % int) ix
                pyFunc "create" ["cls", arg] rv $ do
                    "return cls._create(records) # type: ignore"
        AsterixExpansion cat (A.Edition a b) expan -> do
            ref <- walk expan
            out $ pyClass "Asterix" ix "AstRef" $ do
                fmt ("cv_category = " % int) (coerce cat :: Int)
                fmt ("cv_edition = (" % int % ", " % int % ")") a b
                fmt ("cv_expansion: TypeAlias = Expansion_" % int) ref

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
            AsterixBasic cat ed _     -> ("CAT", (cat, (ed, nameOfAst spec)))
            AsterixExpansion cat ed _ -> ("REF", (cat, (ed, nameOfAst spec)))

    go :: Text -> [BlockM Builder ()]
    go t = do
        let candidates = [b | (a, b) <- lst, a == t]
        cat <- sort $ nub $ fmap fst candidates
        let hdr = fmt (int % ": [") (coerce cat :: Int)
        pure $ do
            hdr
            indent $ mconcat $ do
                cls <- snd <$> sortOn fst [b | (a, b) <- candidates, a == cat]
                pure $ fmt stext (cls <> ",")
            "],"

mkCode :: IsTestSpecs -> AstSpecsRef -> AstSpecsDate
        -> CodeGeneratorVersion -> [A.Asterix] -> Builder
-- | Source code generator entry point.
mkCode isTestSpecs asRef asDate cgVer specs' = render "    " "\n" $ do
    "# Asterix specifications" :: BlockM Builder ()
    ""
    "# This file is generated, DO NOT EDIT!"
    "# For more details, see:"
    "#     - https://github.com/zoranbosnjak/asterix-specs"
    ""
    "from asterix.base import *"
    unless isTestSpecs $ do
        ""
        line $ "asterix_specs_ref = " <> BL.fromText (quote asRef)
        line $ "asterix_specs_date = " <> BL.fromText (quote asDate)
        line $ "code_generator_version = " <> BL.fromText (quote cgVer)
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
