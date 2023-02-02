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
import Bluebook.ManPath
import Bluebook.Shake
import Data.FileEmbed
import qualified UnliftIO.Directory as UnliftIO

main :: IO ()
main = runShake $ do
    manPath <- getEnvManPath
    manifest <- liftIO $ foldMapM loadManifestIO manPath

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
        writeFileBS out $ renderHtmlManPage page html

    "man*/index.html" %> \out -> do
        let section = takeDirectory out
            title = pack $ section <> " man-pages"
        m <- foldMapM (loadManifest section) manPath
        writeFileBS out $ renderHtmlIndex title $ Manifest.toList m

    "index.html" %> \out -> do
        let title = "All man-pages"
        m <- foldMapM (loadManifest "") manPath
        writeFileBS out $ renderHtmlIndex title $ Manifest.toList m

    "404.html" %> \out -> do
        writeFileBS out $ renderHtml "Not Found" notFoundHtml

    "css/main.css" %> (`writeFileBS` css)

    phony "clean" $ liftIO $ removeFiles "." ["//*.html", "//*.css"]

loadManifest :: FilePath -> FilePath -> Action Manifest
loadManifest = loadManifestVia getDirectoryFiles

loadManifestIO :: FilePath -> IO Manifest
loadManifestIO = loadManifestVia getDirectoryFilesIO ""

loadManifestVia
    :: MonadIO m
    => (FilePath -> [FilePath] -> m [FilePath])
    -> FilePath
    -> FilePath
    -> m Manifest
loadManifestVia getFiles subdir path = do
    exists <- UnliftIO.doesDirectoryExist path
    contents <- if exists then getFiles path [subdir <> "//*"] else pure []
    pure $ addBluebook $ Manifest.fromList $ mapMaybe (newManPage path) contents
  where
    addBluebook
        | subdir `elem` ["", "man1"] = Manifest.addBluebook
        | otherwise = id

css :: ByteString
css = $(embedFile "bluebook.css")
