{-# LANGUAGE TupleSections #-}

module Bluebook.Convert
    ( ManPageError
    , manPageErrorText
    , ManPageHtml
    , manPageTitle
    , manPageBody
    , tryManPage2Html
    , tryMarkdown2ManPage
    , findManPage
    , readManPage
    ) where

import Bluebook.Prelude

import Bluebook.Artifact
import Bluebook.Html
import Bluebook.ManPage
import Bluebook.ManPage.Name
import Bluebook.ManPage.Section
import Bluebook.ManPath
import qualified Bluebook.Pandoc as Pandoc
import qualified Codec.Compression.GZip as GZip
import Control.Monad.Except (throwError)
import Control.Monad.Extra (findM)
import qualified Data.Text as T
import System.FilePath (takeExtension, (<.>), (</>))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Html (class_)
import UnliftIO.Directory (doesFileExist)

data ManPageError
    = ManPageNotFound
    | PandocError Pandoc.PandocError

manPageErrorText :: ManPageError -> Text
manPageErrorText = \case
    ManPageNotFound -> "Not found"
    PandocError e -> Pandoc.renderError e

data ManPageHtml = ManPageHtml
    { manPage :: ManPage
    , manPageTitle :: Text
    , manPageBody :: Html
    }

instance ToArtifact ManPageHtml where
    toArtifact ManPageHtml {..} = Artifact
        { artifactPath = unpack $ manPageUrlPath manPage
        , artifactContent = defaultLayout title manPageBody
        }
        where title = "Bluebook - " <> manPageTitle

tryManPage2Html
    :: (MonadIO m, MonadLogger m, MonadError ManPageError m)
    => ManPage
    -> Text
    -> m ManPageHtml
tryManPage2Html page body = do
    (meta, html) <- do
        result <- liftIO $ Pandoc.runIO $ do
            Pandoc.setVerbosity Pandoc.ERROR

            doc <- Pandoc.readMan Pandoc.def body
            let Pandoc.Pandoc meta _ = doc

            fmap (meta, )
                . Pandoc.writeHtml5 Pandoc.def
                . Pandoc.walk Pandoc.addHeaderLinks
                . Pandoc.walk Pandoc.reduceHeaderLevels
                . Pandoc.walk Pandoc.convertManPageRefs
                . Pandoc.walk Pandoc.linkBareUrls
                $ doc

        either (throwError . PandocError) pure result

    let mDate = textMetaValue <$> Pandoc.lookupMeta "date" meta
        mTitle = textMetaValue <$> Pandoc.lookupMeta "title" meta
        title = maybe defTitle (<> sectionRef section) mTitle

    logDebug $ "man2html" :# ["input" .= body, "metadata" .= show @Text meta]

    pure $ ManPageHtml
        { manPage = page
        , manPageTitle = title
        , manPageBody = do
            Html.section ! Html.class_ "man-page" $ do
                Html.header $ Html.h1 $ Html.toHtml title
                html
                Html.footer $ do
                    Html.ul $ do
                        Html.li $ toHtml $ sectionName section
                        Html.li $ toHtml $ fromMaybe "-" mDate
                        Html.li $ toHtml title
        }
  where
    section = manPageSection page
    defTitle = T.toUpper $ manPageToRef page

tryMarkdown2ManPage
    :: (MonadIO m, MonadError ManPageError m) => ByteString -> m Text
tryMarkdown2ManPage md = do
    result <- liftIO $ Pandoc.runIO $ do
        Pandoc.setVerbosity Pandoc.ERROR
        doc <- Pandoc.readMarkdown readOptions $ decodeUtf8 @Text md
        Pandoc.writeMan Pandoc.def doc
    either (throwError . PandocError) pure result
  where
    readOptions = Pandoc.def
        { Pandoc.readerStandalone = True
        , Pandoc.readerExtensions = Pandoc.githubMarkdownExtensions
        }

-- | We only work with 'MetaInlines' because we know that's all we need
textMetaValue :: Pandoc.MetaValue -> Text
textMetaValue = \case
    Pandoc.MetaInlines is -> mconcat $ map go is
    _ -> ""
  where
    go = \case
        Pandoc.Str x -> x
        Pandoc.Space -> " "
        _ -> ""

findManPage
    :: (MonadIO m, MonadError ManPageError m, MonadReader env m, HasManPath env)
    => ManPage
    -> m FilePath
findManPage page = do
    manPath <- view manPathL
    let candidates = concatMap toPaths manPath
    maybe (throwError ManPageNotFound) pure =<< findM doesFileExist candidates
  where
    section = manPageSection page
    name = manPageName page
    path =
        sectionPath section </> unpack (getName name) <> sectionSuffix section
    toPaths dir = [dir </> path, dir </> path <.> "gz"]

readManPage :: MonadIO m => FilePath -> m Text
readManPage path = decodeUtf8 . toStrict . decompress <$> readFileLBS path
  where
    decompress
        | takeExtension path == ".gz" = GZip.decompress
        | otherwise = id
