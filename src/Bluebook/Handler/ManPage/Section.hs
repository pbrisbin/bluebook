{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Bluebook.Handler.ManPage.Section
    ( SectionAPI
    , handleSection
    ) where

import Bluebook.Prelude

import Bluebook.Handler.Html
import Bluebook.Handler.ManPage
import Bluebook.Listing
import Bluebook.ManPage.Name
import Bluebook.ManPage.Section
import Bluebook.Settings
import Servant
import Servant.HTML.Blaze

-- brittany-disable-next-binding

type SectionAPI
       = QueryParam "q" Text :> Get '[HTML] Html
    :<|> Capture "page" Name :> ManPageAPI

handleSection :: Settings -> Section -> Server SectionAPI
handleSection settings section =
    handleGetSection settings section :<|> handleManPage settings section

handleGetSection :: Settings -> Section -> Maybe Text -> Handler Html
handleGetSection settings section mQ = do
    let s = QueryBySection section
        q = maybe s ((s <>) . QueryByName) mQ
    listing <- buildListing settings q
    let title = "Bluebook - " <> pack (sectionPath section)
    pure $ defaultLayout title $ listingToHtml listing
