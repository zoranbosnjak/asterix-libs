-- | Asterix source code (lib) generator.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                (Text)
import qualified Data.Text
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy.Builder   as T
import qualified Data.Text.Lazy.IO        as TL
import           Data.Version             (showVersion)
import           Main.Utf8                (withUtf8)
import           Options.Applicative      as Opt
import           Paths_generator          (version)

import           Asterix.Specs            (Asterix, cDecoder)
import           Asterix.Specs.Syntaxes   (syntaxes)
import           Asterix.Specs.Validation (runErrM, validate)

import qualified Language.Python
import           Types

languages :: [(Target, Generator)]
languages =
    [ ("python", Language.Python.mkCode)
    ]

data Options = Options
    { optLanguage     :: Target
    , optTest         :: IsTestSpecs
    , optAstSpecsRef  :: AstSpecsRef
    , optAstSpecsDate :: AstSpecsDate
    , optPaths        :: [FilePath]
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> strOption (long "language" <> metavar "LANG"
        <> help ("Target format: " ++ show (fmap fst languages)))
    <*> switch (long "test"
       <> help "Generate test output")
    <*> strOption
        ( long "ast-specs-ref" <> metavar "REF"
       <> help "Version control reference of asterix specs"
       <> showDefault
       <> value "unknown"
        )
    <*> strOption
        ( long "ast-specs-date" <> metavar "DATE"
       <> help "Version control date of asterix specs"
       <> showDefault
       <> value "unknown"
        )
    <*> some (Opt.argument str (metavar "PATH..."
       <> help ("Spec input file(s), supported formats: " ++ show syntaxList)))
  where
    syntaxList = do
        (shortName, coder) <- syntaxes
        case cDecoder coder of
            Nothing -> empty
            Just _  -> pure shortName

ownVersion :: String
ownVersion = case showVersion version of
    "" -> "devel"
    s  -> s

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> parseOptions)
    ( fullDesc <> Opt.header "aspecs" )
  where
    versionOption = Opt.infoOption ownVersion
        (Opt.long "version" <> Opt.help "Show version")

loadSpec :: (FilePath -> IO Text) -> FilePath -> IO Asterix
loadSpec getS path = do
    syntax <- maybe (die "unknown syntax") pure (lookup fmt syntaxes)
    decoder <- maybe (die "no decoder") pure (cDecoder syntax)
    s <- getS path
    ast <- either die pure (decoder path s)
    case runErrM (validate ast) of
        []   -> pure ast
        _lst -> die "validation errors"
  where
    die :: String -> IO a
    die msg = fail $ path <> ": " <> msg
    fmt = reverse $ takeWhile (/= '.') (reverse path)

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    specs <- mapM (loadSpec T.readFile) (optPaths cmdOptions)
    mkCode <- maybe (fail "Unsupported language") pure $
        lookup (optLanguage cmdOptions) languages
    TL.putStr $ T.toLazyText $ mkCode
        (optTest cmdOptions)
        (optAstSpecsRef cmdOptions)
        (optAstSpecsDate cmdOptions)
        (Data.Text.pack ownVersion)
        specs
