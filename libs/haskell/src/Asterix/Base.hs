-- | Base asterix processing

module Asterix.Base where

import           Control.Applicative
import           Control.Monad
import           Data.Function       (fix)
import           Data.Maybe
import           Data.Text           (Text)
import           Prelude

import           Asterix.Parsing
import           Asterix.Schema
import           Bits

data Variation bs
    = Element bs Int Int VContent
    | Group bs [Item bs]
    | Extended bs [Maybe (Item bs)]
    | Repetitive bs [Variation bs]
    | Explicit bs
    | Compound bs [Item bs]
    deriving (Eq, Show, Functor)

data Item bs
    = Spare bs Int Int
    | Item Text Text (Variation bs)
    deriving (Eq, Show, Functor)

newtype Record bs = Record (Variation bs)
    deriving (Eq, Show, Functor)

-- | Use type parameter, to avoid runtime check for the common case (single).
data Datablock (single :: Bool) bs where
    DatablockSingle :: [Record bs] -> Datablock 'True bs
    DatablockMultiple :: [(Text, Record bs)] -> Datablock 'False bs

deriving instance Eq bs => Eq (Datablock single bs)
deriving instance Show bs => Show (Datablock single bs)
deriving instance Functor (Datablock single)

-- | Parse something, return actual bits, together with the result.
parseBitsWith :: Parsing a -> Parsing (Bits, a)
parseBitsWith act = do
    s1 <- get
    result <- act
    s2 <- get
    let n = Bits.length s1 - Bits.length s2
    pure (Bits.take n s1, result)

parseVariation :: VVariation -> Parsing (Variation Bits)
parseVariation = \case
    VElement o n vcont ->
        Element <$> fetch n <*> pure o <*> pure n <*> pure vcont
    VGroup lst -> do
        (s, items) <- parseBitsWith (mapM parseItem lst)
        pure (Group s items)
    VExtended ts tss -> do
        let go = \case
                [] -> pure []
                (Just i : xs) -> (:) <$> fmap Just (parseItem i) <*> go xs
                (Nothing : xs) -> do
                    fx <- fetchBool
                    case fx of
                        False -> pure [Nothing]
                        True  -> (:) <$> pure Nothing <*> go xs
        (s, lst) <- parseBitsWith $ go (ts <> join tss)
        pure $ Extended s lst
    VRepetitive mn var -> case mn of
        Nothing -> do
            let go = do
                    x <- parseVariation var
                    fx <- fetchBool
                    case fx of
                        False -> pure [x]
                        True  -> (:) <$> pure x <*> go
            (s, vars) <- parseBitsWith go
            pure $ Repetitive s vars
        Just n1 -> do
            (s, vars) <- parseBitsWith $ do
                n2 <- Bits.getNumberAligned <$> fetch (n1*8)
                sequence (replicate n2 $ parseVariation var)
            pure $ Repetitive s vars
    VExplicit -> do
        (s, _) <- parseBitsWith $ do
            n <- fmap fromIntegral fetchWord8
            when (n <= 0) $ throw ExplicitError
            fetch (8 * pred n)
        pure $ Explicit s
    VCompound mn lst -> do
        (s, items) <- parseBitsWith $ do
            fspec' <- case mn of
                Nothing -> fix $ \loop -> do
                    w <- fmap unpackWord8 fetchWord8
                    let (flags, fx) = (init w, last w)
                    case fx of
                        False -> pure flags
                        True  -> (<>) <$> pure flags <*> loop
                Just n -> do
                    ws <- sequence (replicate n fetchWord8)
                    pure $ join $ fmap unpackWord8 ws
            let fspec = reverse $ dropWhile (== False) $ reverse fspec'
            when (Prelude.length fspec > Prelude.length lst) $ throw FxError
            forM (zip fspec lst) $ \case
                (False, _) -> pure Nothing
                (True, Nothing) -> throw ItemPresenceError
                (True, Just i) -> Just <$> parseItem i
        pure $ Compound s $ catMaybes items

parseItem :: VItem -> Parsing (Item Bits)
parseItem = \case
    VSpare o n ->
        Spare <$> fetch n <*> pure o <*> pure n
    VItem name title var ->
        Item <$> pure name <*> pure title <*> parseVariation var

-- | Single UAP datablock parser.
parseDatablock :: VVariation -> Parsing (Datablock 'True Bits)
parseDatablock var = eof >>= \case
    True -> pure $ DatablockSingle []
    False -> do
        x <- parseVariation var
        parseDatablock var >>= \case
            DatablockSingle lst -> pure $ DatablockSingle (Record x : lst)

-- | Try to parse with different UAPs, where each record in a datablock
-- can potentially be of a different UAP.
parseDatablockTryWith :: Bits -> [(Text, VVariation)] -> [Datablock 'False Bits]
parseDatablockTryWith s probes
    | Bits.null s = pure $ DatablockMultiple []
    | otherwise = do
        (name, var) <- probes
        case runParser (parseVariation var) s of
            Left _ -> empty
            Right (v, s') -> parseDatablockTryWith s' probes >>= \case
                DatablockMultiple lst -> pure $ DatablockMultiple ((name, Record v) : lst)
