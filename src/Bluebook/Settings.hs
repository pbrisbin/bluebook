module Bluebook.Settings
    ( Settings(..)
    , loadSettings

    -- * Access
    , HasRenderLink(..)
    , HasManPath(..)

    -- * Utilities
    , getDefaultManPath
    ) where

import Bluebook.Prelude

import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import Bluebook.RenderLink
import qualified Env
import qualified System.Environment.XDG.BaseDir as XDG

data Settings = Settings
    { settingsPort :: Int
    , settingsAppRoot :: Maybe Text
    , settingsLogSettings :: LogSettings
    , settingsManPath :: [FilePath]
    }

loadSettings :: MonadIO m => m Settings
loadSettings = do
    manPath <- getDefaultManPath

    liftIO
        $ Env.parse (Env.header "Build and serve local man-pages as HTML")
        $ Settings
        <$> Env.var Env.auto "PORT" (Env.def 3000)
        <*> optional (Env.var (Env.str <=< Env.nonempty) "APPROOT" mempty)
        <*> LoggingEnv.parser
        <*> Env.var
                (Env.splitOn ':' <=< Env.nonempty)
                "MANPATH"
                (Env.def manPath)

getDefaultManPath :: MonadIO m => m [FilePath]
getDefaultManPath = liftIO $ do
    manDataDir <- XDG.getUserDataDir "man"
    pure [manDataDir, "/usr/local/share/man", "/usr/share/man"]

class HasManPath env where
    manPathL :: Lens' env [FilePath]

instance HasManPath [FilePath] where
    manPathL = id

instance HasManPath Settings where
    manPathL = lens settingsManPath $ \x y -> x { settingsManPath = y }
