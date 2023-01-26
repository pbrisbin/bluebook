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
import Bluebook.Settings
import Control.Monad.Except (withExceptT)
import qualified Data.List.NonEmpty as NE
import Options.Applicative
import Servant (ServerError(..))
import System.FilePath (isAbsolute, takeDirectory, (<.>), (</>))
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import UnliftIO.Directory (createDirectoryIfMissing, getCurrentDirectory)

data Options = Options
    { oOut :: FilePath
    , oAppRoot :: Maybe Text
    , oManPath :: Maybe (NonEmpty FilePath)
    }

-- brittany-disable-next-binding

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        (  short 'o'
        <> long "out"
        <> help "TODO"
        <> metavar "PATH"
        <> action "directory"
        <> value "dist"
        )
    <*> optional (strOption
        (  long "app-root"
        <> help "TODO"
        <> metavar "PATH|URL"
        ))
    <*> optional (NE.some1 (strOption
        (  long "man-path"
        <> help "TODO"
        <> metavar "PATH"
        <> action "directory"
        )))

data App = App
    { appLogger :: Logger
    , appOut :: FilePath
    , appAppRoot :: Text
    , appManPath :: [FilePath]
    }

instance HasLogger App where
    loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasAppRoot App where
    appRootL = lens appAppRoot $ \x y -> x { appAppRoot = y }

instance HasManPath App where
    manPathL = lens appManPath $ \x y -> x { appManPath = y }

loadApp :: Options -> IO App
loadApp Options {..} = do
    cwd <- getCurrentDirectory

    let defaultAppRoot = pack $ if isAbsolute oOut then oOut else cwd </> oOut

    App
        <$> newLoggerEnv
        <*> pure oOut
        <*> pure (fromMaybe defaultAppRoot oAppRoot)
        <*> maybe getDefaultManPath (pure . NE.toList) oManPath

run :: Options -> IO ()
run options = do
    app <- loadApp options

    let writeHtml relativePath html = do
            let path = appOut app </> relativePath
            logInfo $ "Writing HTML" :# ["path" .= path]
            createDirectoryIfMissing True $ takeDirectory path
            writeFileLBS path $ Blaze.renderHtml html

        writeIndex relativePath =
            writeHtml $ relativePath </> "index" <.> "html"

    flip runAppT app $ do
        writeIndex "" =<< handleGetRoot Nothing
        for_ [minBound .. maxBound] $ \section -> do
            writeIndex (sectionPath section)
                =<< handleGetSection section Nothing

        pages <- listingManPages <$> buildListing QueryAll
        logInfo $ "Discovered individual man-pages" :# ["count" .= length pages]

        for_ pages $ \page -> do
            let section = manPageSection page
                name = manPageName page
                directory = appOut app </> sectionPath section
                suffixed = unpack name <> sectionSuffix section
                path = directory </> suffixed <.> "html"

            result <- runExceptT $ do
                suffixedName <- hoistEither $ mkName $ pack suffixed
                withExceptT errReasonPhrase
                    $ handleGetManPage (manPageSection page) suffixedName

            case result of
                Left err -> do
                    logWarn
                        $ "Error processing man-page"
                        :# ["page" .= manPageToRef page, "error" .= err]
                Right html -> writeHtml path html
