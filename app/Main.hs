module Main
    ( main
    ) where

import Bluebook.Prelude

import Bluebook.API
import Bluebook.App
import Bluebook.Settings

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    settings <- loadSettings
    app <- loadApp settings
    runAPI (settingsPort settings) app
