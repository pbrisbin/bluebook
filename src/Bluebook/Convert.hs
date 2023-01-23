module Bluebook.Convert
    ( ManPageError
    , manPageErrorText
    , ManPageHtml
    , manPageTitle
    , manPageBody
    , tryManPage2Html
    ) where

import           Bluebook.Prelude

import           Bluebook.ManPage
import qualified Bluebook.Pandoc        as Pandoc
import           Bluebook.Settings
import qualified Codec.Compression.GZip as GZip
import qualified Data.List.NonEmpty     as NE
import           System.FilePath        (takeExtension, (<.>), (</>))
import           Text.Blaze.Html        (Html)
import qualified Text.Blaze.Html5       as Html
import           UnliftIO.Directory     (doesFileExist)

newtype ManPageError = PandocError Pandoc.PandocError

manPageErrorText :: ManPageError -> Text
manPageErrorText = \case
    PandocError e -> Pandoc.renderError e

data ManPageHtml = ManPageHtml
    { manPageTitle :: Text
    , manPageBody  :: Html
    }

tryManPage2Html
    :: MonadIO m
    => Settings
    -> FilePath
    -> m (Maybe (Either ManPageError ManPageHtml))
tryManPage2Html Settings {..} name = do
    mPaths <- fmap NE.nonEmpty $ filterM doesFileExist $ concatMap
        (\dir -> [dir </> name, dir </> name <.> "gz"])
        settingsManPath

    for mPaths $ \paths -> runPandoc $ do
        let path = head paths
            title =
                maybe (pack path) manPageToRef $ hush $ manPageFromFile path

        man <- readManPage path
        doc <- Pandoc.readMan
            Pandoc.def { Pandoc.readerExtensions = Pandoc.emptyExtensions }
            man
        body <-
            Pandoc.writeHtml5 Pandoc.def
                { Pandoc.writerExtensions = Pandoc.emptyExtensions
                }
            . Pandoc.walk Pandoc.addHeaderLinks
            . Pandoc.walk Pandoc.reduceHeaderLevels
            . Pandoc.walk Pandoc.convertManPageRefs
            . Pandoc.walk Pandoc.linkBareUrls
            $ doc

        pure $ ManPageHtml
            { manPageTitle = title
            , manPageBody = do
                Html.header $ Html.h1 $ Html.toHtml title
                body
            }

readManPage :: MonadIO m => FilePath -> m Text
readManPage path = decodeUtf8 . toStrict . decompress <$> readFileLBS path
  where
    decompress
        | takeExtension path == ".gz" = GZip.decompress
        | otherwise = id

runPandoc :: MonadIO m => Pandoc.PandocIO c -> m (Either ManPageError c)
runPandoc f = liftIO $ first PandocError <$> Pandoc.runIO f
