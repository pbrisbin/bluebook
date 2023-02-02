module Bluebook.Settings
    ( Settings(..)
    , loadSettings
    ) where

import Bluebook.Prelude

import qualified Data.Text as T
import qualified Env
import qualified System.Environment.XDG.BaseDir as XDG

data Settings = Settings
    { root :: Text
    , manPath :: [FilePath]
    }

-- brittany-disable-next-binding

loadSettings :: MonadIO m => m Settings
loadSettings = liftIO $ do
    manDataDir <- XDG.getUserDataDir "man"
    let mp = [manDataDir, "/usr/local/share/man", "/usr/share/man"]

    Env.parse (Env.header "BLUEBOOK(1)") $ Settings
        <$> Env.var (fmap ensureSlash . Env.nonempty) "HTML_ROOT" (Env.def "/")
        <*> Env.var (Env.splitOn ':' <=< Env.nonempty) "MANPATH" (Env.def mp)

ensureSlash :: Text -> Text
ensureSlash r = if "/" `T.isSuffixOf` r then r else r <> "/"
