module Bluebook.Manifest
    ( Manifest
    , fromList
    , toList
    , lookup
    , lookupThrow
    , lookupRef
    , addBluebook
    ) where

import Bluebook.Prelude hiding (fromList, toList)

import Bluebook.Error
import Bluebook.ManPage (ManPage)
import qualified Bluebook.ManPage as ManPage
import qualified Data.Map.Strict as Map
import Data.Semigroup.Generic
import UnliftIO.Exception (throwIO)

data Manifest = Manifest
    { outputs :: Map FilePath ManPage
    , refs :: Map Text ManPage
    }
    deriving stock (Eq, Show, Generic)
    deriving (Semigroup, Monoid) via GenericSemigroupMonoid Manifest

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

addBluebook :: Manifest -> Manifest
addBluebook = (<> fromList [ManPage.bluebook])
