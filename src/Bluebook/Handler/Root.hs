module Bluebook.Handler.Root
    ( handleGetRoot
    ) where

import Bluebook.Prelude

import Bluebook.Handler.Html
import Bluebook.Listing
import Bluebook.Settings

handleGetRoot
    :: (MonadIO m, MonadReader env m, HasAppRoot env, HasManPath env)
    => Maybe Text
    -> m Html
handleGetRoot =
    defaultLayout "Bluebook"
        <=< listingToHtml
        <=< buildListing
        . maybe QueryAll QueryByName
