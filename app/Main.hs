module Main
    ( main
    ) where

import Bluebook.Prelude

import qualified Bluebook.API as API
import qualified Bluebook.CLI as CLI
import Options.Applicative

data Command
    = RunAPI
    | RunCLI CLI.Options

-- brittany-disable-next-binding

parseCommand :: IO Command
parseCommand = execParser
    $ withInfo "Convert, save, or serve man-pages as HTML"
    $ subparser
    $  command "serve" (withInfo "Serve local man-pages" (pure RunAPI))
    <> command "write" (withInfo "Write local man-pages" (RunCLI <$> CLI.optionsParser))
    where withInfo x p = info (p <**> helper) $ fullDesc <> progDesc x

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    parseCommand >>= \case
        RunAPI -> API.run
        RunCLI options -> CLI.run options
