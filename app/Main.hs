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
import Bluebook.ManPage (readSectionThrow)
import qualified Bluebook.ManPage as ManPage
import Bluebook.ManPath
import Bluebook.Shake
import Data.Aeson (encode)
import Data.FileEmbed
import UnliftIO.Directory (createDirectoryIfMissing)

main :: IO ()
main = runShake $ do
    manifest <- loadAndSaveManifests

    "man*" </> "*.*" <.> "html" %> \out -> do
        page <- findFile (`Manifest.lookup` manifest) out
        content <- ManPage.read readFileBS page
        html <- manPage2Html manifest content `actionCatch` \e -> do
            putWarn $ displayException e
            pure $ errorHtml e
        writeFileBS out $ renderHtmlManPage page html

    want $ map ManPage.outputPath $ Manifest.toList manifest

    "man*" </> "index" <.> "html" %> \out -> do
        section <- readSectionThrow $ takeDirectory out

        let title = "Man-pages in Section " <> show section
            pages = Manifest.toList $ Manifest.filterSection section manifest

        need [out -<.> "json"]
        writeFileBS out $ renderHtmlIndex title pages

    want $ map (\n -> "man" <> show @_ @Int n </> "index" <.> "html") [1 .. 8]

    "index" <.> "html" %>! \out -> do
        let title = "All man-pages"
            pages = Manifest.toList manifest

        need [out -<.> "json"]
        writeFileBS out $ renderHtmlIndex title pages

    "css" </> "main" <.> "css" %>! (`writeFileBS` css)

    "404" <.> "html" %>! \out -> do
        writeFileBS out $ renderHtml "Not Found" notFoundHtml

    phony "clean" $ liftIO $ removeFiles "." ["//*"]

loadAndSaveManifests :: MonadIO m => m Manifest
loadAndSaveManifests = do
    manPath <- getEnvManPath
    m <- Manifest.addBluebook <$> foldMapM Manifest.load manPath

    for_ [1 .. 8] $ \n -> do
        let json = "man" <> show n </> "index" <.> "json"
        createDirectoryIfMissing True $ takeDirectory json
        writeFileLBS json $ encode $ Manifest.filterSection n m

    m <$ writeFileLBS ("index" <.> "json") (encode m)

css :: ByteString
css = $(embedFile "bluebook.css")
