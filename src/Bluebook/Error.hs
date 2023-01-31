module Bluebook.Error
    ( Error(..)
    , runPandoc
    ) where

import Bluebook.Prelude

import Text.Pandoc.Class as Pandoc
import Text.Pandoc.Error as Pandoc
import Text.Pandoc.Logging as Pandoc
import UnliftIO.Exception (throwIO)

data Error
    = FileNotFound FilePath
    | PandocError Pandoc.PandocError
    deriving stock Show

instance Exception Error where
    displayException = \case
        FileNotFound path -> "File not found: " <> path
        PandocError e ->
            "Error in Pandoc conversion: " <> unpack (Pandoc.renderError e)

runPandoc :: MonadIO m => Pandoc.PandocIO a -> m a
runPandoc f = do
    result <- liftIO $ Pandoc.runIO $ Pandoc.setVerbosity Pandoc.ERROR >> f
    either (throwIO . PandocError) pure result
