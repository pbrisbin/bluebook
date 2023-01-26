module Bluebook.CLI
    ( Options
    , optionsParser
    , run
    ) where

import Bluebook.Prelude

import Blammo.Logging.Simple
import Bluebook.App (runAppT)
import Bluebook.Convert
import Bluebook.Html
import Bluebook.Listing
import Bluebook.ManPage
import Bluebook.ManPage.Section
import Bluebook.ManPath
import Options.Applicative
import System.FilePath ((<.>), (</>), takeDirectory)
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)

newtype Options = Options
    { oOut :: FilePath
    }

optionsParser :: Parser Options
optionsParser = Options <$> strOption
    (mconcat
        [ short 'o'
        , long "out"
        , help "Write man-pages into this directory"
        , metavar "PATH"
        , action "directory"
        , value "dist"
        ]
    )

data App = App
    { appLogger :: Logger
    , appManPath :: [FilePath]
    , appOut :: FilePath
    }

instance HasLogger App where
    loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasManPath App where
    manPathL = lens appManPath $ \x y -> x { appManPath = y }

loadApp :: Options -> IO App
loadApp Options {..} = App <$> newLoggerEnv <*> getEnvManPath <*> pure oOut

data Artifact = Artifact
    { artifactPath :: FilePath
    , artifactContent :: Html
    }

writeArtifact :: (MonadIO m, MonadLogger m) => FilePath -> Artifact -> m ()
writeArtifact out Artifact {..} = do
    exists <- doesFileExist absPath
    if exists
        then logDebug $ "Exists" :# ["path" .= absPath]
        else do
            logInfo $ "Write" :# ["path" .= absPath]
            createDirectoryIfMissing True $ takeDirectory absPath
            writeFileLBS absPath $ Blaze.renderHtml artifactContent
    where absPath = out </> artifactPath

run :: Options -> IO ()
run options = do
    app <- loadApp options

    flip runAppT app $ do
        listing <- buildListing

        let write = writeArtifact $ appOut app

        write $ rootArtifact listing
        traverse_ (write . sectionArtifact listing) [minBound .. maxBound]

        for_ (listingManPages listing) $ \page -> do
            result <-
                runExceptT
                $ tryManPage2Html page
                =<< readManPage
                =<< findManPage page

            either
                (\e ->
                    logError
                        $ "Failure"
                        :# [ "page" .= manPageToRef page
                           , "error" .= manPageErrorText e
                           ]
                )
                (write . manPageArtifact page)
                result

rootArtifact :: Listing -> Artifact
rootArtifact listing = Artifact
    { artifactPath = "index" <.> "html"
    , artifactContent = defaultLayout "Bluebook" $ listingToHtml listing
    }

sectionArtifact :: Listing -> Section -> Artifact
sectionArtifact listing section = Artifact
    { artifactPath = sectionPath section </> "index" <.> "html"
    , artifactContent = defaultLayout title $ listingToHtml $ filterListing
        (QueryBySection section)
        listing
    }
    where title = "Bluebook - " <> pack (sectionPath section)

manPageArtifact :: ManPage -> ManPageHtml -> Artifact
manPageArtifact page mp = Artifact
    { artifactPath = unpack $ manPageUrlPath page
    , artifactContent = defaultLayout title $ manPageBody mp
    }
    where title = "Bluebook - " <> manPageTitle mp
