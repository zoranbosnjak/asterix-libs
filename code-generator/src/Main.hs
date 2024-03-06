-- | Asterix source code (lib) generator.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Text                (Text)
import qualified Data.Text.IO             as T
import           Data.Text.Lazy.Builder   (Builder)
import qualified Data.Text.Lazy.Builder   as T
import qualified Data.Text.Lazy.IO        as TL
import qualified Data.Time.Calendar
import qualified Data.Time.Clock          as Clock
import qualified Data.Time.Clock.POSIX    as Time
import           Data.Version             (showVersion)
import           Formatting               as F
import           Main.Utf8                (withUtf8)
import           Options.Applicative      as Opt
import           Paths_generator          (version)
import           System.Exit              (exitSuccess)

import           Asterix.Specs
import           Asterix.Specs.Syntaxes
import           Asterix.Specs.Validation (runErrM, validate)

import qualified Language.Haskell
import qualified Language.Python

languages :: [(Text, Bool -> Text -> Text -> [Asterix] -> Builder)]
languages =
    [ ("python", Language.Python.mkCode)
    , ("haskell", Language.Haskell.mkCode)
    ]

data Options = Options
    { optLanguage    :: Text
    , optTimestamp   :: Integer
    , optReference   :: Text
    , optTest        :: Bool
    , optPaths       :: [FilePath]
    , optShowVersion :: Bool
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> strOption (long "language" <> metavar "LANG"
        <> help ("Target format: " ++ show (fmap fst languages)))
    <*> option auto
        ( long "timestamp" <> metavar "TS"
       <> help "Version unix timestamp"
       <> showDefault
       <> value 0
        )
    <*> strOption
        ( long "reference" <> metavar "REF"
       <> help "Version control reference"
       <> showDefault
       <> value "unknown"
        )
    <*> switch (long "test"
       <> help "Generate test output")
    <*> some (Opt.argument str (metavar "PATH..."
       <> help ("Spec input file(s), supported formats: " ++ show syntaxList)))
    <*> switch (long "show-version"
       <> help "Show version text and exit")
  where
    syntaxList = do
        (shortName, coder) <- syntaxes
        case cDecoder coder of
            Nothing -> empty
            Just _  -> pure shortName

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> parseOptions)
    ( fullDesc <> Opt.header "aspecs" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
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

toVersionText :: Integer -> Text
toVersionText unixtime = sformat
    (int % left 2 '0' % left 2 '0' % "." % int)
    year month day (seconds :: Integer)
  where
    Clock.UTCTime uDay uTime = Time.posixSecondsToUTCTime $ fromIntegral unixtime
    (year, month, day) = Data.Time.Calendar.toGregorian uDay
    seconds = round uTime

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    let versionText = toVersionText $ optTimestamp cmdOptions
    when (optShowVersion cmdOptions) $ do
        T.putStr versionText
        exitSuccess
    specs <- mapM (loadSpec T.readFile) (optPaths cmdOptions)
    mkCode <- maybe (fail "Unsupported language") pure $
        lookup (optLanguage cmdOptions) languages
    TL.putStr $ T.toLazyText $ mkCode
        (optTest cmdOptions) (optReference cmdOptions) versionText specs
