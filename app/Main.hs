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
import Bluebook.Shake
import Data.Aeson (encode)
import Data.FileEmbed

main :: IO ()
main = do
    let manifestPath = "manifest" <.> "json"
    manifest <- loadAndSaveManifest manifestPath

    runShake $ do
        "man*" </> "*.*" <.> "html" %> \out -> do
            page <- findFile (`Manifest.lookup` manifest) out
            content <- ManPage.read readFileBS page
            html <- manPage2Html manifest content `actionCatch` \e -> do
                putWarn $ displayException e
                pure $ errorHtml e
            writeFileBS out $ renderHtmlManPage page html

        want $ map ManPage.outputPath $ Manifest.toList manifest

        "man*" </> "index" <.> "html" %> \out -> do
            need [manifestPath]
            let section = takeDirectory out
                pages = filter (ManPage.inSectionPath section)
                    $ Manifest.toList manifest
            writeFileBS out $ renderHtmlIndex (Just $ pack section) pages

        want
            [ "man1" </> "index" <.> "html"
            , "man2" </> "index" <.> "html"
            , "man3" </> "index" <.> "html"
            , "man4" </> "index" <.> "html"
            , "man5" </> "index" <.> "html"
            , "man6" </> "index" <.> "html"
            , "man7" </> "index" <.> "html"
            , "man8" </> "index" <.> "html"
            ]

        "index" <.> "html" %>! \out -> do
            need [manifestPath]
            let pages = Manifest.toList manifest
            writeFileBS out $ renderHtmlIndex Nothing pages

        "css" </> "main" <.> "css" %>! (`writeFileBS` css)

        "404" <.> "html" %>! (`writeFileBS` notFoundHtml)

        phony "clean" $ liftIO $ removeFiles "." ["//*"]

loadAndSaveManifest :: FilePath -> IO Manifest
loadAndSaveManifest path = do
    manPath <- getEnvManPath
    m <- Manifest.addBluebook <$> foldMapM Manifest.load manPath
    m <$ writeFileLBS path (encode m)

css :: ByteString
css = $(embedFile "bluebook.css")
