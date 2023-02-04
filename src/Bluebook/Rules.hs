{-# LANGUAGE TemplateHaskell #-}

module Bluebook.Rules
    ( rules
    ) where

import Bluebook.Prelude hiding (readFileBS, writeFileBS)

import Bluebook.Convert
import Bluebook.Html
import Bluebook.Manifest (Manifest)
import qualified Bluebook.Manifest as Manifest
import qualified Bluebook.ManPage as ManPage
import Bluebook.Settings
import Bluebook.Shake
import Data.FileEmbed

rules :: Settings -> Manifest -> Rules ()
rules Settings {..} manifest = do
    "man*/*.*.html" %> \out -> do
        page <- Manifest.lookupThrow out manifest
        content <- ManPage.read readFileBS page
        (html, _) <- man2Html (Just manifest) content `actionCatch` \e -> do
            putWarn $ displayException e
            pure (errorHtml e, pageDetailsFromPath out)
        writeFileBS out $ renderHtml root "man-page" html

    "man*/index.html" %> \out -> do
        let section = takeDirectory out
            title = pack $ section <> " man-pages"
        m <- Manifest.load getDirectoryFiles root section manPath
        writeFileBS out $ renderHtmlIndex root title $ Manifest.toList m

    "index.html" %> \out -> do
        let title = "All man-pages"
        m <- Manifest.load getDirectoryFiles root "" manPath
        writeFileBS out $ renderHtmlIndex root title $ Manifest.toList m

    "404.html" %> \out -> do
        writeFileBS out $ renderHtml root "Not Found" notFoundHtml

    "css/main.css" %> (`writeFileBS` css)

css :: ByteString
css = $(embedFile "bluebook.css")
