module Bluebook.Handler.Root
    ( handleGetRoot
    ) where

import Bluebook.Prelude

import Bluebook.Handler.Html
import Bluebook.Listing
import Bluebook.Settings

handleGetRoot :: Settings -> Maybe Text -> Handler Html
handleGetRoot settings mQ = do
    let q = maybe QueryAll QueryByName mQ
    listing <- buildListing settings q
    pure $ defaultLayout "Bluebook" $ listingToHtml listing
