{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Bluebook.Prelude hiding (readFileBS, writeFileBS)

import Bluebook.Convert
import Bluebook.Error
import Bluebook.Html
import Bluebook.Manifest (Manifest)
import qualified Bluebook.Manifest as Manifest
import qualified Bluebook.ManPage as ManPage
import Bluebook.ManPath
import Bluebook.Options
import Bluebook.Shake
import Data.Aeson (encode)
import Data.FileEmbed

main :: IO ()
main = do
    options@Options {..} <- parseOptions

    let undist = dropPrefix $ dist <> [pathSeparator]
        manifestPath = dist </> "manifest" <.> "json"

    manifest <- loadAndSaveManifest manifestPath

    runShake options $ do
        dist </> "man*" </> "*.*" <.> "html" %> \out -> do
            page <- findFile (`Manifest.lookup` manifest) $ undist out
            content <- ManPage.read readFileBS page
            html <- manPage2Html manifest content `actionCatch` \e -> do
                putWarn $ displayException e
                pure $ errorHtml e
            writeFileBS out $ renderHtmlManPage page html

        want $ map ((dist </>) . ManPage.outputPath) $ Manifest.toList manifest

        dist </> "man*" </> "index" <.> "html" %> \out -> do
            need [manifestPath]
            let section = takeDirectory $ undist out
                pages = filter (ManPage.inSectionPath section)
                    $ Manifest.toList manifest
            writeFileBS out $ renderHtmlIndex (Just $ pack section) pages

        want
            [ dist </> "man1" </> "index" <.> "html"
            , dist </> "man2" </> "index" <.> "html"
            , dist </> "man3" </> "index" <.> "html"
            , dist </> "man4" </> "index" <.> "html"
            , dist </> "man5" </> "index" <.> "html"
            , dist </> "man6" </> "index" <.> "html"
            , dist </> "man7" </> "index" <.> "html"
            , dist </> "man8" </> "index" <.> "html"
            ]

        dist </> "index" <.> "html" %>! \out -> do
            need [manifestPath]
            let pages = Manifest.toList manifest
            writeFileBS out $ renderHtmlIndex Nothing pages

        dist </> "css" </> "main" <.> "css" %>! (`writeFileBS` css)

        dist </> "404" <.> "html" %>! (`writeFileBS` notFoundHtml)

        phony "clean" $ liftIO $ removeFiles dist ["//*"]

loadAndSaveManifest :: FilePath -> IO Manifest
loadAndSaveManifest path = do
    manPath <- getEnvManPath
    m <- Manifest.addBluebook <$> foldMapM Manifest.load manPath
    m <$ writeFileLBS path (encode m)

css :: ByteString
css = $(embedFile "bluebook.css")
