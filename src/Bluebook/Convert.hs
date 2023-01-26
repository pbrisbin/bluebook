{-# LANGUAGE TupleSections #-}

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
import Control.Monad.Except (throwError, withExceptT)
import Control.Monad.Extra (findM)
import System.FilePath (takeExtension, (<.>), (</>))
import Text.Blaze.Html (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Html (class_)
import UnliftIO.Directory (doesFileExist)

data ManPageError
    = PandocError Pandoc.PandocError
    | MissingMetaError Text
    | InvalidMetaError Text String

manPageErrorText :: ManPageError -> Text
manPageErrorText = \case
    PandocError e -> Pandoc.renderError e
    MissingMetaError k -> "Key " <> k <> " not found in document metadata"
    InvalidMetaError k err ->
        "Key " <> k <> " in document metadata is invalid: " <> pack err

data ManPageHtml = ManPageHtml
    { manPageTitle :: Text
    , manPageBody :: Html
    }

tryManPage2Html
    :: (MonadIO m, MonadLogger m, MonadReader env m, HasRenderLink env)
    => Text
    -> m (Either ManPageError ManPageHtml)
tryManPage2Html body = do
    rl <- view renderLinkL
    runExceptT $ do
        (meta, html) <-
            withExceptT PandocError $ ExceptT $ liftIO $ Pandoc.runIO $ do
                doc <- Pandoc.readMan Pandoc.def body
                let Pandoc.Pandoc meta _ = doc

                fmap (meta, )
                    . Pandoc.writeHtml5 Pandoc.def
                    . Pandoc.walk Pandoc.addHeaderLinks
                    . Pandoc.walk Pandoc.reduceHeaderLevels
                    . Pandoc.walk (Pandoc.convertManPageRefs rl)
                    . Pandoc.walk Pandoc.linkBareUrls
                    $ doc

        date <- textMetaValue <$> requireMeta "date" meta
        title <- textMetaValue <$> requireMeta "title" meta
        section <- sectionFromMetaValue =<< requireMeta "section" meta

        let titleRef = title <> sectionRef section

        logDebug
            $ "man2html"
            :# ["input" .= body, "metadata" .= show @Text meta]

        pure $ ManPageHtml
            { manPageTitle = titleRef
            , manPageBody = do
                Html.header $ Html.h1 $ Html.toHtml titleRef
                html
                Html.ul ! Html.class_ "man-page-footer" $ do
                    Html.li $ toHtml $ sectionName section
                    Html.li $ toHtml date
                    Html.li $ toHtml titleRef
            }

requireMeta
    :: MonadError ManPageError m => Text -> Pandoc.Meta -> m Pandoc.MetaValue
requireMeta k = maybe err pure . Pandoc.lookupMeta k
    where err = throwError $ MissingMetaError k

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

sectionFromMetaValue
    :: MonadError ManPageError m => Pandoc.MetaValue -> m Section
sectionFromMetaValue =
    either (throwError . InvalidMetaError "section") pure
        . sectionFromSuffix
        . unpack
        . ("." <>)
        . textMetaValue

findManPage
    :: (MonadIO m, MonadReader env m, HasManPath env)
    => Section
    -> Name
    -> m (Maybe FilePath)
findManPage section name = do
    manPath <- view manPathL
    findM doesFileExist $ concatMap toPaths manPath
  where
    path = sectionPath section </> unpack (getName name)
    toPaths dir = [dir </> path, dir </> path <.> "gz"]

readManPage :: MonadIO m => FilePath -> m Text
readManPage path = decodeUtf8 . toStrict . decompress <$> readFileLBS path
  where
    decompress
        | takeExtension path == ".gz" = GZip.decompress
        | otherwise = id
