{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Bluebook.Prelude hiding (readFileBS, writeFileBS)

import Bluebook.Convert
import Bluebook.Html
import Bluebook.Manifest (Manifest)
import qualified Bluebook.Manifest as Manifest
import Bluebook.ManPage (newManPage)
import qualified Bluebook.ManPage as ManPage
import Bluebook.Settings
import Bluebook.Shake
import Data.FileEmbed
import qualified UnliftIO.Directory as UnliftIO

main :: IO ()
main = do
    Settings {..} <- loadSettings

    runShake $ do
        manifest <- liftIO $ foldMapM (loadManifestIO root) manPath

        want $ concat
            [ map (\n -> "man" <> show @_ @Int n <> "/index.html") [1 .. 8]
            , map ManPage.outputPath $ Manifest.toList manifest
            , ["index.html", "404.html", "css/main.css"]
            ]

        "man*/*.*.html" %> \out -> do
            page <- Manifest.lookupThrow out manifest
            content <- ManPage.read readFileBS page
            html <- manPage2Html manifest content `actionCatch` \e -> do
                putWarn $ displayException e
                pure $ errorHtml e
            writeFileBS out $ renderHtmlManPage root page html

        "man*/index.html" %> \out -> do
            let section = takeDirectory out
                title = pack $ section <> " man-pages"
            m <- foldMapM (loadManifest root section) manPath
            writeFileBS out $ renderHtmlIndex root title $ Manifest.toList m

        "index.html" %> \out -> do
            let title = "All man-pages"
            m <- foldMapM (loadManifest root "") manPath
            writeFileBS out $ renderHtmlIndex root title $ Manifest.toList m

        "404.html" %> \out -> do
            writeFileBS out $ renderHtml root "Not Found" notFoundHtml

        "css/main.css" %> (`writeFileBS` css)

        phony "clean" $ liftIO $ removeFiles "." ["//*.html", "//*.css"]

loadManifest :: Text -> FilePath -> FilePath -> Action Manifest
loadManifest = loadManifestVia getDirectoryFiles

loadManifestIO :: Text -> FilePath -> IO Manifest
loadManifestIO root = loadManifestVia getDirectoryFilesIO root ""

loadManifestVia
    :: MonadIO m
    => (FilePath -> [FilePath] -> m [FilePath])
    -> Text
    -> FilePath
    -> FilePath
    -> m Manifest
loadManifestVia getFiles root subdir path = do
    exists <- UnliftIO.doesDirectoryExist path
    contents <- if exists then getFiles path [subdir <> "//*"] else pure []
    pure $ addBluebook $ Manifest.fromList $ mapMaybe
        (newManPage root path)
        contents
  where
    addBluebook
        | subdir `elem` ["", "man1"] = Manifest.addBluebook
        | otherwise = id

css :: ByteString
css = $(embedFile "bluebook.css")
