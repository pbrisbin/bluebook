module Bluebook.Settings
    ( Settings(..)
    , loadSettings
    ) where

import Bluebook.Prelude

data Settings = Settings
    { settingsPort :: Int
    , settingsManPath :: [FilePath]
    }

loadSettings :: MonadIO m => m Settings
loadSettings = do
    port <- lookupEnv "PORT"

    pure Settings
        { settingsPort = fromMaybe 3000 $ readMaybe =<< port
        , settingsManPath =
            [ "/home/patrick/.local/share/man"
            , "/usr/local/share/man"
            , "/usr/share/man"
            ]
        }
