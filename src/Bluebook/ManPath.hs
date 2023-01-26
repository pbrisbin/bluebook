module Bluebook.ManPath
    ( HasManPath(..)
    , getEnvManPath
    ) where

import Bluebook.Prelude

import qualified Env
import qualified System.Environment.XDG.BaseDir as XDG

getEnvManPath :: MonadIO m => m [FilePath]
getEnvManPath = do
    manPath <- getDefaultManPath
    liftIO $ Env.parse (Env.header "BLUEBOOK(1)") $ Env.var
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
