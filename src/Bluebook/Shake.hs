module Bluebook.Shake
    ( runShake
    , readFileBS
    , writeFileBS
    , (%>!)

    -- * Re-export
    , module Development.Shake
    , module Development.Shake.FilePath
    ) where

import Bluebook.Prelude hiding (readFileBS, writeFileBS)
import qualified Bluebook.Prelude as Prelude

import Development.Shake
import Development.Shake.FilePath
import UnliftIO.Directory (createDirectoryIfMissing)

runShake :: Rules () -> IO ()
runShake = shakeArgs defaultShakeOptions

defaultShakeOptions :: ShakeOptions
defaultShakeOptions = shakeOptions
    { shakeThreads = 0
    , shakeVerbosity = Verbose
    , shakeChange = ChangeModtimeAndDigestInput
    , shakeColor = True
    }

readFileBS :: FilePath -> Action ByteString
readFileBS name = do
    need [name]
    Prelude.readFileBS name

writeFileBS :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBS name x = do
    createDirectoryIfMissing True $ takeDirectory name
    Prelude.writeFileBS name x

-- | Build a rule and immediately add it with 'want'
(%>!) :: FilePath -> (FilePath -> Action ()) -> Rules ()
a %>! b = do
    a %> b
    want [a]

infix 1 %>!
