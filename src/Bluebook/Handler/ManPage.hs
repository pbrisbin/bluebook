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

handleManPage :: Settings -> Section -> Name -> Server ManPageAPI
handleManPage = handleGetManPage

handleGetManPage :: Settings -> Section -> Name -> Handler Html
handleGetManPage settings section name = do
    renderMan2Html ref
        =<< readManPage
        =<< fromMaybeNotFound
        =<< findManPage settings section name
    where ref = getName name <> sectionRef section

renderMan2Html :: Text -> Text -> Handler Html
renderMan2Html ref body = do
    page <-
        fromEitherServerError
        . first manPageErrorText
        =<< tryManPage2Html ref body

    let title = "Bluebook - " <> manPageTitle page
    pure $ defaultLayout title $ manPageBody page
