module Bluebook.CLI
    ( Options
    , optionsParser
    , run
    ) where

import Bluebook.Prelude

import Blammo.Logging.Simple
import Bluebook.App (runAppT)
import Bluebook.Artifact
import Bluebook.Convert
import Bluebook.Html
import Bluebook.Listing
import Bluebook.ManPage
import Bluebook.ManPage.Section
import Bluebook.ManPath
import Options.Applicative
import System.FilePath ((<.>), (</>))

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
    , appArtifacts :: FilePath
    }

instance HasLogger App where
    loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasManPath App where
    manPathL = lens appManPath $ \x y -> x { appManPath = y }

instance HasArtifacts App where
    artifactsL = lens appArtifacts $ \x y -> x { appArtifacts = y }

loadApp :: Options -> IO App
loadApp Options {..} = App <$> newLoggerEnv <*> getEnvManPath <*> pure oOut

run :: Options -> IO ()
run options = do
    app <- loadApp options

    flip runAppT app $ do
        pages <- filterM writeManPage =<< buildListing

        writeArtifact $ RootHtml pages

        traverse_
            (writeArtifact . flip toSectionHtml pages)
            [minBound .. maxBound]

writeManPage
    :: ( MonadMask m
       , MonadIO m
       , MonadLogger m
       , MonadReader env m
       , HasManPath env
       , HasArtifacts env
       )
    => ManPage
    -> m Bool
writeManPage page = withThreadContext ["page" .= manPageToRef page] $ do
    result <-
        runExceptT $ tryManPage2Html page =<< readManPage =<< findManPage page

    case result of
        Left err -> False <$ logError (manPageErrorText err :# [])
        Right html -> True <$ writeArtifact html

newtype RootHtml = RootHtml
    { rootPages :: [ManPage]
    }

instance ToArtifact RootHtml where
    toArtifact RootHtml {..} = Artifact
        { artifactPath = "index" <.> "html"
        , artifactContent = defaultLayout "Bluebook"
            $ listingToHtml "All man-pages" rootPages
        }

data SectionHtml = SectionHtml
    { section :: Section
    , sectionPages :: [ManPage]
    }

instance ToArtifact SectionHtml where
    toArtifact SectionHtml {..} = Artifact
        { artifactPath = sectionPath section </> "index" <.> "html"
        , artifactContent = defaultLayout title $ listingToHtml
            ("Section " <> show (sectionNumber section) <> " man-pages")
            sectionPages
        }
        where title = "Bluebook - " <> pack (sectionPath section)

toSectionHtml :: Section -> [ManPage] -> SectionHtml
toSectionHtml section =
    SectionHtml section . filter ((== section) . manPageSection)
