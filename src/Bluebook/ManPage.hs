{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Bluebook.ManPage
    ( ManPage(..)
    , newManPage
    , read
    , bluebook
    ) where

import Bluebook.Prelude

import Bluebook.Error
import qualified Codec.Compression.GZip as GZip
import Data.FileEmbed
import System.FilePath (splitDirectories, takeExtension, (</>))
import Text.Pandoc.Options as Pandoc
import Text.Pandoc.Readers.Markdown as Pandoc
import Text.Pandoc.Writers.Man as Pandoc

data ManPage = ManPage
    { sourcePath :: FilePath
    , outputPath :: FilePath
    , section :: Int
    , name :: Text
    , url :: Text
    , ref :: Text
    }
    deriving stock (Show, Eq, Generic)

newManPage :: FilePath -> FilePath -> Maybe ManPage
newManPage parent path = do
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
        , url = pack $ "/" <> dir <> "/" <> fname <> ".html"
        , ref = name <> "(" <> show section <> ")"
        }

readSection :: FilePath -> Maybe Int
readSection = guarded (`elem` [1 .. 8]) <=< readMaybe <=< stripPrefix "man"

read :: MonadIO m => (FilePath -> m ByteString) -> ManPage -> m Text
read f page
    | ref page == ref bluebook = bluebookContent
    | isGZip = decodeUtf8 . decompress <$> f (sourcePath page)
    | otherwise = decodeUtf8 <$> f (sourcePath page)
  where
    isGZip = takeExtension (sourcePath page) == ".gz"
    decompress = toStrict . GZip.decompress . fromStrict

bluebook :: ManPage
bluebook = ManPage
    { sourcePath = "/dev/null" -- ignored
    , outputPath = "man1/bluebook.1.html"
    , section = 1
    , name = "bluebook"
    , url = "/man1/bluebook.1.html"
    , ref = "bluebook(1)"
    }

bluebookContent :: MonadIO m => m Text
bluebookContent = runPandoc $ do
    doc <- Pandoc.readMarkdown readOptions $ decodeUtf8 @Text readmeMd
    Pandoc.writeMan Pandoc.def doc
  where
    readOptions = Pandoc.def
        { Pandoc.readerStandalone = True
        , Pandoc.readerExtensions = Pandoc.githubMarkdownExtensions
        }

readmeMd :: ByteString
readmeMd = $(embedFile "README.md")
