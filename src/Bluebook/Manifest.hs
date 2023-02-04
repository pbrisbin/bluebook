module Bluebook.Manifest
    ( Manifest
    , load
    , fromList
    , toList
    , lookup
    , lookupThrow
    , lookupRef
    ) where

import Bluebook.Prelude hiding (fromList, toList)

import Bluebook.Error
import Bluebook.ManPage (ManPage, newManPage)
import qualified Bluebook.ManPage as ManPage
import Bluebook.Shake (FilePattern)
import qualified Data.Map.Strict as Map
import Data.Semigroup.Generic
import UnliftIO.Directory (doesDirectoryExist)
import UnliftIO.Exception (throwIO)

data Manifest = Manifest
    { outputs :: Map FilePath ManPage
    , refs :: Map Text ManPage
    }
    deriving stock (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via GenericSemigroupMonoid Manifest

load
    :: MonadIO m
    => (FilePath -> [FilePattern] -> m [FilePath])
    -- ^ Function to read contents of a directory matching patterns
    --
    -- This is injected so we can use 'getDirectoryFilesIO' when we don't want
    -- the files tracked as a dependency, but 'getDirectoryFiles' when we do.
    --
    -> Text
    -- ^ App root to use when constructing 'ManPage' values
    -> FilePath
    -- ^ A Sub-directory to scope in (e.g. @man1@), or empty string
    -> [FilePath]
    -> m Manifest
load getFiles root subdir = foldMapM go
  where
    go path = do
        exists <- doesDirectoryExist path
        contents <- if exists then getFiles path [subdir <> "//*"] else pure []
        pure $ fromList $ mapMaybe (newManPage root path) contents

fromList :: [ManPage] -> Manifest
fromList pages = Manifest
    { outputs = mapBy ManPage.outputPath
    , refs = mapBy ManPage.ref
    }
  where
    mapBy :: Ord a => (ManPage -> a) -> Map a ManPage
    mapBy f = Map.fromList $ map (f &&& id) pages

toList :: Manifest -> [ManPage]
toList = Map.elems . outputs -- arbitrary which we use

lookup :: FilePath -> Manifest -> Maybe ManPage
lookup path = Map.lookup path . outputs

lookupThrow :: MonadIO m => FilePath -> Manifest -> m ManPage
lookupThrow path = maybe (throwIO $ FileNotFound path) pure . lookup path

lookupRef :: Text -> Manifest -> Maybe ManPage
lookupRef ref = Map.lookup ref . refs
