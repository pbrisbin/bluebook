{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Bluebook.Handler.ManPage.Section
    ( SectionAPI
    , handleSection
    , handleGetSection
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

handleSection
    :: ( MonadIO m
       , MonadLogger m
       , MonadError ServerError m
       , MonadReader env m
       , HasRenderLink env
       , HasManPath env
       )
    => Section
    -> ServerT SectionAPI m
handleSection section = handleGetSection section :<|> handleManPage section

handleGetSection
    :: (MonadIO m, MonadReader env m, HasRenderLink env, HasManPath env)
    => Section
    -> Maybe Text
    -> m Html
handleGetSection section =
    defaultLayout title <=< listingToHtml <=< buildListing . maybe
        s
        ((s <>) . QueryByName)
  where
    s = QueryBySection section
    title = "Bluebook - " <> pack (sectionPath section)
