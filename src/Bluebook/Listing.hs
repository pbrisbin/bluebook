module Bluebook.Listing
    ( buildListing
    , listingToHtml
    ) where

import Bluebook.Prelude

import Bluebook.ManPage
import Bluebook.ManPage.Section
import Bluebook.ManPath
import System.FilePath ((</>))
import Text.Blaze.Html5
    (Html, a, h2, header, li, section, toHtml, toValue, ul, (!))
import Text.Blaze.Html5.Attributes (href)
import UnliftIO.Directory (doesDirectoryExist, listDirectory)

listingToHtml :: Text -> [ManPage] -> Html
listingToHtml title pages = do
    header $ h2 $ toHtml title
    section $ ul $ traverse_ listItem pages
  where
    listItem page =
        li
            $ a
            ! href (toValue $ "/" <> manPageUrlPath page)
            $ toHtml
            $ manPageToRef page

buildListing :: (MonadIO m, MonadReader env m, HasManPath env) => m [ManPage]
buildListing = do
    manPath <- view manPathL
    results <- for [minBound .. maxBound] $ \s -> do
        for manPath $ \mp -> do
            let prefix = sectionPath s
                manDir = mp </> prefix

            exists <- doesDirectoryExist manDir
            if exists
                then map (prefix </>) <$> listDirectory manDir
                else pure []

    pure $ sort $ rights $ map manPageFromFile $ concat $ concat results

-- data Query
--     = QueryAll
--     | QueryBySection Section
--     | QueryByName Text
--     | QueryAnd Query Query

-- instance Semigroup Query where
--     (<>) = QueryAnd

-- queryToText :: Query -> Text
-- queryToText = \case
--     QueryAll -> "all man-pages"
--     QueryBySection s ->
--         "man-pages in section " <> pack (show $ sectionNumber s)
--     QueryByName t -> "man-pages with " <> t <> " in the name"
--     QueryAnd q1 q2 -> queryToText q1 <> ", " <> queryToText q2

-- matchesQuery :: ManPage -> Query -> Bool
-- matchesQuery mp = \case
--     QueryAll -> True
--     QueryBySection s -> manPageSection mp == s
--     QueryByName t -> t `T.isInfixOf` getName (manPageName mp)
--     QueryAnd q1 q2 -> mp `matchesQuery` q1 && mp `matchesQuery` q2

-- filterListing :: Query -> Listing -> Listing
-- filterListing q listing = listing
--     { listingQuery = q
--     , listingManPages = filter (`matchesQuery` q) $ listingManPages listing
--     }
