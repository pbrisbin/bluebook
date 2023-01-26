module Bluebook.Handler.Ping
    ( handleGetPing
    ) where

import Bluebook.Prelude

import Bluebook.Handler.Html
import Bluebook.Settings
import qualified Text.Blaze.Html5 as Html

handleGetPing :: (MonadReader env m, HasAppRoot env) => m Html
handleGetPing = defaultLayout "Hello world" $ do
    Html.header $ Html.h1 "Hello world"
    Html.p "This service is up."
