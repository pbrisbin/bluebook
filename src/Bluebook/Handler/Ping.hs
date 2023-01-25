module Bluebook.Handler.Ping
    ( handleGetPing
    ) where

import Bluebook.Prelude

import Bluebook.Handler.Html
import qualified Text.Blaze.Html5 as Html

handleGetPing :: Applicative m => m Html
handleGetPing = pure $ defaultLayout "Hello world" $ do
    Html.header $ Html.h1 "Hello world"
    Html.p "This service is up."
