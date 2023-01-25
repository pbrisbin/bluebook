module Bluebook.Convert
    ( ManPageError
    , manPageErrorText
    , ManPageHtml
    , manPageTitle
    , manPageBody
    , tryManPage2Html
    , findManPage
    , readManPage
    ) where

import Bluebook.Prelude

import Bluebook.ManPage.Name
import Bluebook.ManPage.Section
import qualified Bluebook.Pandoc as Pandoc
import Bluebook.Settings
import qualified Codec.Compression.GZip as GZip
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T
import System.FilePath (takeExtension, (<.>), (</>))
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as Html
import UnliftIO.Directory (doesFileExist)

newtype ManPageError = PandocError Pandoc.PandocError

manPageErrorText :: ManPageError -> Text
manPageErrorText = \case
    PandocError e -> Pandoc.renderError e

data ManPageHtml = ManPageHtml
    { manPageTitle :: Text
    , manPageBody :: Html
    }

tryManPage2Html
    :: MonadIO m
    => Text -- ^ Title
    -> Text -- ^ Content
    -> m (Either ManPageError ManPageHtml)
tryManPage2Html title body = runPandoc $ do
    liftIO $ T.putStrLn $ "[man2html]: input=" <> body
    doc <- Pandoc.readMan Pandoc.def body
    liftIO $ T.putStrLn $ "[man2html]: output=" <> pack (show doc)
    html <-
        Pandoc.writeHtml5 Pandoc.def
        . Pandoc.walk Pandoc.addHeaderLinks
        . Pandoc.walk Pandoc.reduceHeaderLevels
        . Pandoc.walk Pandoc.convertManPageRefs
        . Pandoc.walk Pandoc.linkBareUrls
        $ doc

    pure $ ManPageHtml
        { manPageTitle = title
        , manPageBody = do
            Html.header $ Html.h1 $ Html.toHtml title
            html
        }

findManPage :: MonadIO m => Settings -> Section -> Name -> m (Maybe FilePath)
findManPage Settings {..} section name =
    fmap (fmap NE.head . NE.nonEmpty)
        . filterM doesFileExist
        . concatMap toPaths
        $ settingsManPath
  where
    path = sectionPath section </> unpack (getName name)
    toPaths dir = [dir </> path, dir </> path <.> "gz"]

readManPage :: MonadIO m => FilePath -> m Text
readManPage path = decodeUtf8 . toStrict . decompress <$> readFileLBS path
  where
    decompress
        | takeExtension path == ".gz" = GZip.decompress
        | otherwise = id

runPandoc :: MonadIO m => Pandoc.PandocIO c -> m (Either ManPageError c)
runPandoc f = liftIO $ first PandocError <$> Pandoc.runIO f
