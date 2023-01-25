module Bluebook.Handler.Root
    ( handleGetRoot
    ) where

import Bluebook.Prelude

import Bluebook.Handler.Html
import Bluebook.Listing
import Bluebook.Settings

handleGetRoot
    :: (MonadIO m, MonadReader env m, HasManPath env) => Maybe Text -> m Html
handleGetRoot =
    fmap (defaultLayout "Bluebook" . listingToHtml)
        . buildListing
        . maybe QueryAll QueryByName
