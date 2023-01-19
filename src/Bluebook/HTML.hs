module Bluebook.HTML
    ( layout
    ) where

import Bluebook.Prelude hiding (head)

import qualified Bluebook.CSS as CSS
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes (charset)

layout :: Text -> Html -> Html
layout t inner = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        title $ toHtml t
        style $ toHtml CSS.styles
    body inner
