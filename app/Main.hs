module Main
    ( main
    ) where

import Bluebook.Prelude

import qualified Bluebook.CLI as CLI
import Options.Applicative

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    CLI.run =<< parseOptions CLI.optionsParser

parseOptions :: Parser options -> IO options
parseOptions = execParser . withInfo "Convert man-pages to HTML"
    where withInfo x p = info (p <**> helper) $ fullDesc <> progDesc x
