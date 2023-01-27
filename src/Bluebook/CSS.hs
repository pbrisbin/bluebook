{-# LANGUAGE TemplateHaskell #-}

module Bluebook.CSS
    ( write
    , stylesPath
    , stylesUrl
    ) where

import Bluebook.Prelude

import Bluebook.Artifact
import Data.FileEmbed
import System.FilePath ((<.>), (</>))

write
    :: (MonadIO m, MonadLogger m, MonadReader env m, HasArtifacts env) => m ()
write = writeArtifactRaw stylesPath $ fromStrict styles

stylesPath :: FilePath
stylesPath = "css" </> "main" <.> "css"

stylesUrl :: Text
stylesUrl = "css/main.css"

styles :: ByteString
styles = $(embedFile "bluebook.css")
