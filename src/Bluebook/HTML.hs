module Bluebook.HTML
    ( layout
    ) where

import Bluebook.Prelude hiding (head)

import qualified Bluebook.CSS as CSS
import Bluebook.ManPage.Section
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5 (Html, docTypeHtml, toHtml, toValue, (!))
import qualified Text.Blaze.Html5.Attributes as Html (charset, href)

layout :: Text -> Html -> Html
layout title body = docTypeHtml $ do
    Html.head $ do
        Html.meta ! Html.charset "UTF-8"
        Html.title $ toHtml title
        Html.style $ toHtml CSS.styles
    Html.body $ do
        Html.nav $ do
            Html.ul $ do
                Html.li $ (Html.a ! Html.href "/") "Home"
                for_ [minBound .. maxBound] $ \section -> do
                    let path = sectionPath section
                        link = toValue $ "/" <> path
                    Html.li $ (Html.a ! Html.href link) $ toHtml path

        body

        Html.footer $ do
            "Built with "
            (Html.a ! Html.href "https://github.com/pbrisbin/bluebook")
                "Bluebook"
            " © 2023 Patrick Brisbin"
