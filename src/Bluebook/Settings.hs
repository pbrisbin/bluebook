module Bluebook.Settings
    ( Settings(..)
    , loadSettings
    ) where

import           Bluebook.Prelude

import qualified Env
import qualified System.Environment.XDG.BaseDir as XDG

data Settings = Settings
    { settingsPort    :: Int
    , settingsManPath :: [FilePath]
    }

loadSettings :: MonadIO m => m Settings
loadSettings = do
    manPath <- getDefaultManPath

    liftIO
        $ Env.parse (Env.header "Build and serve local man-pages as HTML")
        $ Settings
        <$> Env.var Env.auto "PORT" (Env.def 3000)
        <*> Env.var
                (Env.splitOn ':' <=< Env.nonempty)
                "MANPATH"
                (Env.def manPath)

getDefaultManPath :: MonadIO m => m [FilePath]
getDefaultManPath = liftIO $ do
    manDataDir <- XDG.getUserDataDir "man"
    pure [manDataDir, "/usr/local/share/man", "/usr/share/man"]
