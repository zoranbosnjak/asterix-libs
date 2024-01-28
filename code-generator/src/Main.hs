-- | Asterix source code (lib) generator.

module Main where

import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.Text                (Text)
import qualified Data.Text.IO             as T
import           Data.Text.Lazy.Builder   (Builder)
import qualified Data.Text.Lazy.Builder   as BL
import qualified Data.Text.Lazy.Encoding  as TL
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
import           Asterix.Specs.Validation (validate)

import qualified Language.Python

languages :: [(Text, Text -> Text -> [Asterix] -> Builder)]
languages =
    [ ("python", Language.Python.mkCode)
    ]

data Options = Options
    { optLanguage    :: Text
    , optTimestamp   :: Integer
    , optReference   :: Text
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
    <*> some (Opt.argument str (metavar "PATH..."
       <> help ("Spec input file(s), supported formats: " ++ show syntaxList)))
    <*> switch (long "show-version"
       <> help "Show version text and exit" )
  where
    syntaxList = do
        (shortName, _, _) <- availableDecoders
        pure shortName

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> parseOptions)
    ( fullDesc <> Opt.header "aspecs" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
        (Opt.long "version" <> Opt.help "Show version")

loadSpec :: (FilePath -> IO ByteString) -> FilePath -> IO Asterix
loadSpec getS path = do
    syntax <- maybe (die "unknown syntax") pure (lookup fmt syntaxes)
    decoder <- maybe (die "no decoder") pure (syntaxDecoder syntax)
    s <- getS path
    ast <- either die pure (decoder path s)
    case validate False ast of
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
    specs <- mapM (loadSpec BS.readFile) (optPaths cmdOptions)
    mkCode <- maybe (fail "Unsupported language") pure $
        lookup (optLanguage cmdOptions) languages
    BSL.putStr $ TL.encodeUtf8 $ BL.toLazyText $ mkCode
        (optReference cmdOptions) versionText specs
