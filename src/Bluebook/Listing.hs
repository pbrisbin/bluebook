module Bluebook.Listing
    ( Listing
    , listingToHtml
    , Query(..)
    , buildListing
    ) where

import Bluebook.Prelude

import Bluebook.ManPage
import Bluebook.ManPage.Section
import Bluebook.Settings
import qualified Data.Text as T
import System.FilePath ((</>))
import Text.Blaze.Html5
    (Html, a, h2, header, li, section, toHtml, toValue, ul, (!))
import Text.Blaze.Html5.Attributes (href)
import UnliftIO.Directory (doesDirectoryExist, listDirectory)

data Listing = Listing
    { listingQuery :: Query
    , listingManPages :: [ManPage]
    }

listingToHtml :: Listing -> Html
listingToHtml listing = do
    header $ h2 $ toHtml $ ("Listing: " <>) $ queryToText $ listingQuery listing
    section $ ul $ traverse_ listItem $ listingManPages listing
  where
    listItem page =
        li $ a ! href (toValue $ manPageUrlPath page) $ toHtml $ manPageToRef
            page

data Query
    = QueryAll
    | QueryBySection Section
    | QueryByName Text
    | QueryAnd Query Query

instance Semigroup Query where
    (<>) = QueryAnd

queryToText :: Query -> Text
queryToText = \case
    QueryAll -> "all man-pages"
    QueryBySection s ->
        "man-pages in section " <> pack (show $ sectionNumber s)
    QueryByName t -> "man-pages with " <> t <> " in the name"
    QueryAnd q1 q2 -> queryToText q1 <> ", " <> queryToText q2

matchesQuery :: ManPage -> Query -> Bool
matchesQuery mp = \case
    QueryAll -> True
    QueryBySection s -> manPageSection mp == s
    QueryByName t -> t `T.isInfixOf` manPageName mp
    QueryAnd q1 q2 -> mp `matchesQuery` q1 && mp `matchesQuery` q2

buildListing :: MonadIO m => Settings -> Query -> m Listing
buildListing Settings {..} q = do
    results <- for [minBound .. maxBound] $ \s -> do
        for settingsManPath $ \manPath -> do
            let prefix = sectionPath s
                manDir = manPath </> prefix

            exists <- doesDirectoryExist manDir
            if exists
                then map (prefix </>) <$> listDirectory manDir
                else pure []

    pure
        $ Listing q
        $ sort
        $ filter (`matchesQuery` q)
        $ rights
        $ map manPageFromFile
        $ concat
        $ concat results
