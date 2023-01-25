{-# LANGUAGE DataKinds #-}

module Bluebook.Handler.ManPage
    ( ManPageAPI
    , handleManPage
    , renderMan2Html
    ) where

import Bluebook.Prelude

import Bluebook.Convert
import Bluebook.Handler.Html
import Bluebook.ManPage.Name
import Bluebook.ManPage.Section
import Bluebook.Settings
import Servant
import Servant.HTML.Blaze

-- brittany-disable-next-binding

type ManPageAPI = Get '[HTML] Html
    -- TODO: /convert

handleManPage
    :: ( MonadIO m
       , MonadLogger m
       , MonadError ServerError m
       , MonadReader env m
       , HasManPath env
       )
    => Section
    -> Name
    -> ServerT ManPageAPI m
handleManPage = handleGetManPage

handleGetManPage
    :: ( MonadIO m
       , MonadLogger m
       , MonadError ServerError m
       , MonadReader env m
       , HasManPath env
       )
    => Section
    -> Name
    -> m Html
handleGetManPage section =
    renderMan2Html <=< readManPage <=< fromMaybeNotFound <=< findManPage section

renderMan2Html
    :: (MonadIO m, MonadLogger m, MonadError ServerError m) => Text -> m Html
renderMan2Html body = do
    page <-
        fromEitherServerError . first manPageErrorText =<< tryManPage2Html body

    let title = "Bluebook - " <> manPageTitle page
    pure $ defaultLayout title $ manPageBody page
