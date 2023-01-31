module Bluebook.Manifest
    ( Manifest
    , load
    , toList
    , lookup
    , lookupRef
    , addBluebook
    ) where

import Bluebook.Prelude hiding (fromList, toList)

import Bluebook.ManPage (ManPage, newManPage)
import qualified Bluebook.ManPage as ManPage
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as Map
import Data.Semigroup.Generic
import System.FilePath ((</>))
import UnliftIO.Directory (doesDirectoryExist, listDirectory)

data Manifest = Manifest
    { outputs :: Map FilePath ManPage
    , refs :: Map Text ManPage
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass ToJSON
    deriving (Semigroup, Monoid) via GenericSemigroupMonoid Manifest

load :: MonadIO m => FilePath -> m Manifest
load path = do
    contents <- concat <$> traverse listSection [1 .. 8]
    pure $ fromList $ mapMaybe (newManPage path) contents
  where
    listSection :: MonadIO m => Int -> m [FilePath]
    listSection n = do
        let section = "man" <> show n
        map (section </>) <$> listDirectoryIfExists (path </> section)

listDirectoryIfExists :: MonadIO m => FilePath -> m [FilePath]
listDirectoryIfExists x = do
    exists <- doesDirectoryExist x
    if exists then listDirectory x else pure []

fromList :: [ManPage] -> Manifest
fromList pages = Manifest
    { outputs = mapBy ManPage.outputPath
    , refs = mapBy ManPage.ref
    }
  where
    mapBy :: Ord a => (ManPage -> a) -> Map a ManPage
    mapBy f = Map.fromList $ map (f &&& id) pages

toList :: Manifest -> [ManPage]
toList = Map.elems . outputs -- arbitrary which to use

lookup :: FilePath -> Manifest -> Maybe ManPage
lookup path = Map.lookup path . outputs

lookupRef :: Text -> Manifest -> Maybe ManPage
lookupRef ref = Map.lookup ref . refs

addBluebook :: Manifest -> Manifest
addBluebook = (<> fromList [ManPage.bluebook])
