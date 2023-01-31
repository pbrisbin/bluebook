module Bluebook.Options
    ( Options(..)
    , toShakeOptions
    , parseOptions
    ) where

import Bluebook.Prelude

import Development.Shake
    (Change(..), ShakeOptions(..), Verbosity(..), shakeOptions)
import Options.Applicative

newtype Options = Options
    { dist :: FilePath
    }

toShakeOptions :: Options -> ShakeOptions
toShakeOptions _ = shakeOptions
    { shakeThreads = 0
    , shakeVerbosity = Verbose
    , shakeChange = ChangeDigest
    , shakeColor = True
    }

parseOptions :: IO Options
parseOptions = execParser $ withInfo "Convert man-pages to HTML" optionsParser
    where withInfo x p = info (p <**> helper) $ fullDesc <> progDesc x

-- brittany-disable-next-binding

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        (  short 'o'
        <> long "out"
        <> help "Write man-pages into this directory"
        <> metavar "PATH"
        <> value "dist"
        <> action "directory"
        )
