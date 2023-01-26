module Bluebook.Artifact
    ( HasArtifacts(..)
    , Artifact(..)
    , ToArtifact(..)
    , writeArtifact
    ) where

import Bluebook.Prelude

import System.FilePath (takeDirectory, (</>))
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)

class HasArtifacts env where
    artifactsL :: Lens' env FilePath

data Artifact = Artifact
    { artifactPath :: FilePath
    , artifactContent :: Html
    }

class ToArtifact a where
    toArtifact :: a -> Artifact

instance ToArtifact Artifact where
    toArtifact = id

writeArtifact
    :: ( MonadIO m
       , MonadLogger m
       , MonadReader env m
       , HasArtifacts env
       , ToArtifact a
       )
    => a
    -> m ()
writeArtifact a = do
    absPath <- (</> artifactPath) <$> view artifactsL
    exists <- doesFileExist absPath
    if exists
        then logDebug $ "Exists" :# ["path" .= absPath]
        else do
            logInfo $ "Write" :# ["path" .= absPath]
            createDirectoryIfMissing True $ takeDirectory absPath
            writeFileLBS absPath $ Blaze.renderHtml artifactContent
    where Artifact {..} = toArtifact a
