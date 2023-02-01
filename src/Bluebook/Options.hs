module Bluebook.Options
    ( Options(..)
    , parseOptions
    ) where

import Bluebook.Prelude

import Options.Applicative

data Options = Options
    { dist :: FilePath
    , targets :: [FilePath]
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
    <*> many (strArgument
        (  metavar "TARGET"
        <> help "Specify Shake target"
        ))
