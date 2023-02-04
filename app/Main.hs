module Main
    ( main
    ) where

import Bluebook.Prelude

import qualified Bluebook.Manifest as Manifest
import qualified Bluebook.ManPage as ManPage
import Bluebook.Rules
import Bluebook.Settings
import Bluebook.Shake

main :: IO ()
main = do
    settings@Settings {..} <- loadSettings

    runShake $ do
        manifest <- liftIO $ Manifest.load getDirectoryFilesIO root "" manPath

        rules settings manifest

        phony "clean" $ liftIO $ removeFiles "." ["//*.html", "//*.css"]

        -- We want each section index
        want $ map (\n -> "man" <> show @_ @Int n <> "/index.html") [1 .. 8]

        -- We want every page we know about
        want $ map ManPage.outputPath $ Manifest.toList manifest

        -- We want the main index, 404, and css files
        want ["index.html", "404.html", "css/main.css"]
