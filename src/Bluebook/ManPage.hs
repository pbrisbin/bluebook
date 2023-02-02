{-# LANGUAGE NamedFieldPuns #-}

module Bluebook.ManPage
    ( ManPage(..)
    , newManPage
    , read
    ) where

import Bluebook.Prelude

import qualified Codec.Compression.GZip as GZip
import System.FilePath (splitDirectories, takeExtension, (</>))

data ManPage = ManPage
    { sourcePath :: FilePath
    , outputPath :: FilePath
    , section :: Int
    , name :: Text
    , url :: Text
    , ref :: Text
    }
    deriving stock (Show, Eq, Generic)

newManPage :: Text -> FilePath -> FilePath -> Maybe ManPage
newManPage root parent path = do
    -- "manN/foo.N[.gz]" => ["manN", "foo.N"]
    [dir, fname] <- pure $ splitDirectories $ dropSuffix ".gz" path

    -- "man[1-8]" => [1-8]
    section <- readSection dir

    -- "foo.N" -> "foo"
    name <- pack <$> stripSuffix ("." <> show section) fname

    pure ManPage
        { sourcePath = parent </> path
        , outputPath = dir </> fname <> ".html"
        , section
        , name
        , url = root <> pack (dir <> "/" <> fname <> ".html")
        , ref = name <> "(" <> show section <> ")"
        }

readSection :: FilePath -> Maybe Int
readSection = guarded (`elem` [1 .. 8]) <=< readMaybe <=< stripPrefix "man"

read :: MonadIO m => (FilePath -> m ByteString) -> ManPage -> m Text
read f page
    | isGZip = decodeUtf8 . decompress <$> f (sourcePath page)
    | otherwise = decodeUtf8 <$> f (sourcePath page)
  where
    isGZip = takeExtension (sourcePath page) == ".gz"
    decompress = toStrict . GZip.decompress . fromStrict
