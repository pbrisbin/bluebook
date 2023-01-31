{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Bluebook.Prelude hiding (readFileBS, writeFileBS)
import qualified Bluebook.Prelude as Prelude

import Bluebook.Convert
import Bluebook.Error
import Bluebook.Html
import qualified Bluebook.Manifest as Manifest
import qualified Bluebook.ManPage as ManPage
import Bluebook.ManPath
import Bluebook.Options
import Data.Aeson (encode)
import Data.FileEmbed
import Development.Shake
import Development.Shake.FilePath
import UnliftIO.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    options@Options {..} <- parseOptions

    let undist = dropPrefix $ dist <> [pathSeparator]
        manifestPath = dist </> "manifest" <.> "json"

    manifest <- do
        manPath <- getEnvManPath
        m <- Manifest.addBluebook <$> foldMapM Manifest.load manPath
        m <$ writeFileLBS manifestPath (encode m)

    shakeArgs (toShakeOptions options) $ do
        dist </> "index" <.> "html" %> \out -> do
            need [manifestPath]
            let pages = Manifest.toList manifest
            writeFileBS out $ renderHtmlIndex Nothing pages

        dist </> "man*" </> "index" <.> "html" %> \out -> do
            need [manifestPath]
            let section = takeDirectory $ undist out
                pages = filter (ManPage.inSectionPath section)
                    $ Manifest.toList manifest
            writeFileBS out $ renderHtmlIndex (Just $ pack section) pages

        dist </> "man*" </> "*.*" <.> "html" %> \out -> do
            page <- findFile (`Manifest.lookup` manifest) $ undist out
            content <- ManPage.read readFileBS page
            html <- manPage2Html manifest content `actionCatch` \e -> do
                putWarn $ displayException e
                pure $ errorHtml e
            writeFileBS out $ renderHtmlManPage page html

        dist </> "css" </> "main" <.> "css" %> (`writeFileBS` css)

        dist </> "404" <.> "html" %> (`writeFileBS` notFoundHtml)

        want
            $ map (dist </>)
            $ ["index" <.> "html", "css" </> "main" <.> "css", "404" <.> "html"]
            <> map
                   (\n -> "man" <> show @_ @Int n </> "index" <.> "html")
                   [1 .. 8]
            <> map ManPage.outputPath (Manifest.toList manifest)

        phony "clean" $ liftIO $ removeFiles dist ["//*"]

css :: ByteString
css = $(embedFile "bluebook.css")

readFileBS :: FilePath -> Action ByteString
readFileBS name = do
    need [name]
    Prelude.readFileBS name

writeFileBS :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBS name x = do
    createDirectoryIfMissing True $ takeDirectory name
    Prelude.writeFileBS name x
