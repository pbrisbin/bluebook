module Bluebook.ManPage
    ( ManPage
    , newManPage
    , manPageSection
    , manPageName
    , manPageFromFile
    , manPageFromRef
    , manPageToRef
    , manPageUrlPath
    ) where

import Bluebook.Prelude

import Bluebook.ManPage.Name
import Bluebook.ManPage.Section
import qualified Data.Text as T
import System.FilePath (takeFileName)

data ManPage = ManPage
    { section :: Section
    , name :: Name
    }
    deriving stock (Eq, Ord, Show)

newManPage :: Section -> Name -> ManPage
newManPage = ManPage

manPageSection :: ManPage -> Section
manPageSection = section

manPageName :: ManPage -> Name
manPageName = name

-- @.../foo.1(.gz)@ -> @{ name:foo, section:1 }@
manPageFromFile :: FilePath -> Either String ManPage
manPageFromFile path =
    ManPage <$> sectionFromSuffix (unpack suffix) <*> mkName name
  where
    (name, suffix) =
        T.breakOn "." $ pack $ dropSuffix ".gz" $ takeFileName path

-- @foo(1)@ -> @{ name:foo, section:1 }@
manPageFromRef :: Text -> Either String ManPage
manPageFromRef t = do
    let (name, ref) = T.breakOn "(" $ T.strip t
    ManPage <$> sectionFromRef ref <*> mkName name

manPageToRef :: ManPage -> Text
manPageToRef ManPage {..} = getName name <> sectionRef section

-- @{ name:foo, section:1 }@ -> @man1/foo.1.html@
manPageUrlPath :: ManPage -> Text
manPageUrlPath ManPage {..} =
    pack (sectionPath section)
        <> "/"
        <> getName name
        <> pack (sectionSuffix section)
        <> ".html"
