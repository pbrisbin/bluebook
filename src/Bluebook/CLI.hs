module Bluebook.CLI
    ( Options
    , optionsParser
    , run
    ) where

import Bluebook.Prelude

import Blammo.Logging.Simple
import Bluebook.App (runAppT)
import Bluebook.Handler.ManPage
import Bluebook.Handler.ManPage.Section
import Bluebook.Handler.Root
import Bluebook.Listing
import Bluebook.ManPage
import Bluebook.ManPage.Name
import Bluebook.ManPage.Section
import Bluebook.RenderLink
import Bluebook.Settings
import qualified Data.List.NonEmpty as NE
import Options.Applicative
import Servant (ServerError(..))
import System.FilePath (takeDirectory, (<.>), (</>))
import qualified Text.Blaze.Html as Html
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import UnliftIO.Directory (createDirectoryIfMissing)

data Options = Options
    { oOut :: FilePath
    , oWebLink :: Bool
    , oAppRoot :: Maybe Text
    , oManPath :: Maybe (NonEmpty FilePath)
    }

-- brittany-disable-next-binding

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        (  short 'o'
        <> long "out"
        <> help "Write man-pages into this directory"
        <> metavar "PATH"
        <> action "directory"
        <> value "dist"
        )
    <*> switch
        (  long "web-links"
        <> help "Render links for later serving, not for file://"
        )
    <*> optional (strOption
        (  long "app-root"
        <> help "Render links with the following root"
        <> metavar "PATH|URL"
        ))
    <*> optional (NE.some1 (strOption
        (  long "man-path"
        <> help "Include man-pages from this path"
        <> metavar "PATH"
        <> action "directory"
        )))

data App = App
    { appLogger :: Logger
    , appOut :: FilePath
    , appRenderLink :: RenderLink
    , appManPath :: [FilePath]
    }

instance HasLogger App where
    loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasRenderLink App where
    renderLinkL = lens appRenderLink $ \x y -> x { appRenderLink = y }

instance HasManPath App where
    manPathL = lens appManPath $ \x y -> x { appManPath = y }

loadApp :: Options -> IO App
loadApp Options {..} = do
    rl <- if oWebLink
        then pure $ makeRenderLinkWeb oAppRoot
        else makeRenderLinkFile oOut oAppRoot

    App
        <$> newLoggerEnv
        <*> pure oOut
        <*> pure rl
        <*> maybe getDefaultManPath (pure . NE.toList) oManPath

data ManPageHtml = ManPageHtml
    { path :: FilePath
    , content :: Html
    }

writeManPageHtml
    :: (MonadIO m, MonadLogger m) => FilePath -> ManPageHtml -> m ()
writeManPageHtml out ManPageHtml {..} = do
    let absPath = out </> path
    logInfo $ "Writing html" :# ["path" .= absPath]
    createDirectoryIfMissing True $ takeDirectory absPath
    writeFileLBS absPath $ Blaze.renderHtml content

run :: Options -> IO ()
run options = do
    app <- loadApp options

    flip runAppT app $ do
        logInfo "Lising man-pages"
        listing <- buildListing QueryAll

        logInfo "Building man-pages to html"
        pages <- concat <$> sequence
            [ pure <$> renderRoot
            , traverse renderSection [minBound .. maxBound]
            , traverse renderManPage $ listingManPages listing
            ]

        traverse_ (writeManPageHtml (appOut app)) pages

renderRoot
    :: (MonadIO m, MonadReader env m, HasRenderLink env, HasManPath env)
    => m ManPageHtml
renderRoot = ManPageHtml ("index" <.> "html") <$> handleGetRoot Nothing

renderSection
    :: (MonadIO m, MonadReader env m, HasRenderLink env, HasManPath env)
    => Section
    -> m ManPageHtml
renderSection section =
    ManPageHtml (sectionPath section </> "index" <.> "html")
        <$> handleGetSection section Nothing

renderManPage
    :: ( MonadIO m
       , MonadLogger m
       , MonadReader env m
       , HasRenderLink env
       , HasManPath env
       )
    => ManPage
    -> m ManPageHtml
renderManPage page = do
    let section = manPageSection page
        name = manPageName page
        suffixed = unpack name <> sectionSuffix section
        suffixedName = either (error . pack) id $ mkName $ pack suffixed

    result <- runExceptT $ handleGetManPage (manPageSection page) suffixedName

    pure ManPageHtml
        { path = sectionPath section </> suffixed <.> "html"
        , content = case result of
            Left err -> Html.unsafeLazyByteString $ errBody err
            Right html -> html
        }
