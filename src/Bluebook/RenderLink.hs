module Bluebook.RenderLink
    ( RenderLink
    , makeRenderLinkWeb
    , makeRenderLinkFile
    , renderLinkToRoot
    , renderLinkToDirectory
    , renderLinkToFile
    , HasRenderLink(..)
    ) where

import Bluebook.Prelude

import System.FilePath (isAbsolute, (</>))
import UnliftIO.Directory (getCurrentDirectory)

data RenderLink = RenderLink
    { renderLinkToRoot :: Text
    , renderLinkToDirectory :: Text -> Text
    , renderLinkToFile :: Text -> Text
    }

makeRenderLinkWeb :: Maybe Text -> RenderLink
makeRenderLinkWeb = \case
    Nothing -> RenderLink
        { renderLinkToRoot = "/"
        , renderLinkToDirectory = id
        , renderLinkToFile = id
        }
    Just root -> RenderLink
        { renderLinkToRoot = root
        , renderLinkToDirectory = (root <>)
        , renderLinkToFile = (root <>)
        }

makeRenderLinkFile :: MonadIO m => FilePath -> Maybe Text -> m RenderLink
makeRenderLinkFile out mRoot = do
    parent <- if isAbsolute out
        then pure out
        else (</> out) <$> getCurrentDirectory

    let root = fromMaybe (pack parent) mRoot

    pure $ RenderLink
        { renderLinkToRoot = root <> "/index.html"
        , renderLinkToDirectory = \x -> root <> x <> "/index.html"
        , renderLinkToFile = \x -> root <> x <> ".html"
        }

class HasRenderLink env where
    renderLinkL :: Lens' env RenderLink

instance HasRenderLink RenderLink where
    renderLinkL = id
