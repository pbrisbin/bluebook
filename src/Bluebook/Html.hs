module Bluebook.Html
    ( defaultLayout

    -- * Re-exports
    , Html
    , toHtml
    , (!)
    ) where

import Bluebook.Prelude
import qualified Text.Blaze.Html5.Attributes as Html hiding (style, title)

import qualified Bluebook.CSS as CSS
import Bluebook.ManPage.Section
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html5 (docTypeHtml, toHtml, toValue, (!))

defaultLayout :: Text -> Html -> Html
defaultLayout title body = docTypeHtml $ do
    Html.head $ do
        Html.meta ! Html.charset "UTF-8"
        Html.title $ toHtml title
        Html.style $ toHtml CSS.styles
    Html.body $ do
        Html.nav $ do
            Html.ul $ do
                Html.li $ (Html.a ! Html.href "/") "Home"
                for_ [minBound .. maxBound] $ \section -> do
                    let name = pack $ sectionPath section
                        link = toValue $ "/" <> name <> "/"
                    Html.li $ (Html.a ! Html.href link) $ toHtml name

        body

        Html.footer $ do
            "Built with "
            (Html.a ! Html.href "https://github.com/pbrisbin/bluebook")
                "Bluebook"
            " © 2023 Patrick Brisbin"
